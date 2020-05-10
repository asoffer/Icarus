#include "compiler/emit_function_call_infrastructure.h"

#include <vector>

#include "base/debug.h"
#include "ir/builder.h"

namespace compiler {

static type::Typed<ir::Value> ResultsToValue(
    type::Typed<ir::Results> const &results) {
  ir::Value val(false);
  type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                   uint32_t, uint64_t, float, double, type::Type const *,
                   ir::EnumVal, ir::FlagsVal, ir::Addr, ir::String, ir::Fn>(
      results.type(), [&](auto tag) -> void {
        using T = typename decltype(tag)::type;
        val     = ir::Value(results->template get<T>(0));
      });
  return type::Typed<ir::Value>(val, results.type());
}


void MakeAllStackAllocations(Compiler *compiler, ast::FnScope const *fn_scope) {
  for (auto *scope : fn_scope->descendants()) {
    if (scope != fn_scope and scope->is<ast::FnScope>()) { continue; }
    for (const auto &[key, val] : scope->decls_) {
      DEBUG_LOG("MakeAllStackAllocations")(key);
      for (auto *decl : val) {
        if (decl->flags() &
            (ast::Declaration::f_IsConst | ast::Declaration::f_IsFnParam)) {
          DEBUG_LOG("MakeAllStackAllocations")
          ("skipping constant/param decl ", decl->id());
          continue;
        }

        DEBUG_LOG("MakeAllStackAllocations")
        ("allocating ", decl->id());

        compiler->data().set_addr(
            decl, compiler->builder().Alloca(compiler->type_of(decl)));
      }
    }
  }
}

void MakeAllDestructions(Compiler *compiler, ast::ExecScope const *exec_scope) {
  // TODO store these in the appropriate order so we don't have to compute this?
  // Will this be faster?
  std::vector<ast::Declaration *> ordered_decls;
  for (auto &[name, decls] : exec_scope->decls_) {
    ordered_decls.insert(ordered_decls.end(), decls.begin(), decls.end());
  }

  // TODO eek, don't use line number to determine destruction order!
  absl::c_sort(ordered_decls, [](ast::Declaration *lhs, ast::Declaration *rhs) {
    return (lhs->range().begin() > rhs->range().begin());
  });

  for (auto *decl : ordered_decls) {
    auto *t = ASSERT_NOT_NULL(compiler->type_of(decl));
    if (not t->HasDestructor()) { continue; }
    compiler->Visit(t, compiler->data().addr(decl), EmitDestroyTag{});
  }
}

// TODO One problem with this setup is that we don't end up calling destructors
// if we exit early, so those need to be handled externally.
void EmitIrForStatements(Compiler *compiler,
                         base::PtrSpan<ast::Node const> span) {
  ICARUS_SCOPE(ir::SetTemporaries(compiler->builder())) {
    for (auto *stmt : span) {
      DEBUG_LOG("EmitIrForStatements")(stmt->DebugString());
      compiler->EmitValue(stmt);
      compiler->builder().FinishTemporariesWith(
          [compiler](type::Typed<ir::Reg> r) {
            compiler->Visit(r.type(), r.get(), EmitDestroyTag{});
          });
      if (compiler->builder().block_termination_state() !=
          ir::Builder::BlockTerminationState::kMoreStatements) {
        break;
      }
    }
  }
}

void CompleteBody(Compiler *compiler, ast::ShortFunctionLiteral const *node,
                  type::Function const *t) {
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  ir::NativeFn ir_func = *ASSERT_NOT_NULL(compiler->data().FindNativeFn(node));

  auto& bldr = compiler->builder();
  ICARUS_SCOPE(ir::SetCurrent(ir_func.get(), &bldr)) {
    bldr.CurrentBlock() = bldr.CurrentGroup()->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      compiler->data().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(compiler, node->body_scope());
    auto results = compiler->EmitValue(node->body());
    bldr.FinishTemporariesWith([compiler](type::Typed<ir::Reg> r) {
      compiler->Visit(r.type(), r.get(), EmitDestroyTag{});
    });
    bldr.SetRet(0,
                type::Typed<ir::Results>(std::move(results), t->output()[0]));
    MakeAllDestructions(compiler, node->body_scope());
    bldr.ReturnJump();
  }

  ir_func->work_item = nullptr;
  ir_func->WriteByteCode();
}

void CompleteBody(Compiler *compiler, ast::FunctionLiteral const *node,
                  type::Function const *t) {
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  ir::NativeFn ir_func = *ASSERT_NOT_NULL(compiler->data().FindNativeFn(node));

  auto& bldr = compiler->builder();
  ICARUS_SCOPE(ir::SetCurrent(ir_func.get(), &bldr)) {
    bldr.CurrentBlock() = bldr.CurrentGroup()->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      compiler->data().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(compiler, node->body_scope());
    if (auto outputs = node->outputs()) {
      for (size_t i = 0; i < outputs->size(); ++i) {
        auto *out_decl = (*outputs)[i]->if_as<ast::Declaration>();
        if (not out_decl) { continue; }
        auto *out_decl_type = ASSERT_NOT_NULL(compiler->type_of(out_decl));
        auto alloc = out_decl_type->is_big() ? bldr.GetRet(i, out_decl_type)
                                             : bldr.Alloca(out_decl_type);

        compiler->data().set_addr(out_decl, alloc);
        if (out_decl->IsDefaultInitialized()) {
          compiler->Visit(out_decl_type, alloc, EmitDefaultInitTag{});
        } else {
          compiler->Visit(
              out_decl_type, alloc,
              ResultsToValue(type::Typed{
                  compiler->EmitValue(out_decl->init_val()), out_decl_type}),
              EmitCopyAssignTag{});
        }
      }
    }

    EmitIrForStatements(compiler, node->stmts());
    MakeAllDestructions(compiler, node->body_scope());

    if (t->output().empty()) {
      // TODO even this is wrong. Figure out the right jumping strategy
      // between here and where you call SetReturn
      bldr.ReturnJump();
    }
  }

  ir_func->work_item = nullptr;
  ir_func->WriteByteCode();
}

void CompleteBody(Compiler *compiler,
                  ast::ParameterizedStructLiteral const *node) {
  NOT_YET();
}

void CompleteBody(Compiler *compiler, ast::Jump const *node) {
  ir::Jump *jmp = ASSERT_NOT_NULL(compiler->data().jump(node));

  ICARUS_SCOPE(ir::SetCurrent(jmp, &compiler->builder())) {
    ASSERT(compiler != nullptr);
    compiler->builder().CurrentBlock() = jmp->entry();
    // TODO arguments should be renumbered to not waste space on const
    // values
    int32_t i = 0;
    if (node->state()) { compiler->data().set_addr(node->state(), ir::Reg::Arg(i++)); }
    for (auto const &param : node->params()) {
      compiler->data().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(compiler, node->body_scope());

    EmitIrForStatements(compiler, node->stmts());

    // TODO it seems like this will be appended after ChooseJump, which means
    // it'll never be executed.
    MakeAllDestructions(compiler, node->body_scope());
  }
  jmp->WriteByteCode();
  jmp->work_item = nullptr;
}

void ProcessExecutableBody(Compiler *c, base::PtrSpan<ast::Node const> nodes,
                           ir::CompiledFn *main_fn) {
  ASSERT(nodes.size() > 0);
  ast::ModuleScope *mod_scope = &nodes.front()->scope()->as<ast::ModuleScope>();
  ICARUS_SCOPE(ir::SetCurrent(main_fn, &c->builder())) {
    MakeAllStackAllocations(c, mod_scope);
    EmitIrForStatements(c, nodes);
    MakeAllDestructions(c, mod_scope);
    // TODO determine under which scenarios destructors can be skipped.

    c->builder().ReturnJump();
  }
  c->CompleteDeferredBodies();
  // main_fn->WriteByteCode();
}
}  // namespace compiler
