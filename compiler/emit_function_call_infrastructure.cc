#include "compiler/emit_function_call_infrastructure.h"

#include <vector>

#include "base/debug.h"
#include "ir/builder.h"

namespace compiler {
namespace {

template <typename T>
struct PushVec : public base::UseWithScope {
  template <typename... Args>
  PushVec(std::vector<T> *vec, Args &&... args) : vec_(vec) {
    vec_->emplace_back(std::forward<Args>(args)...);
  }

  ~PushVec() { vec_->pop_back(); }

 private:
  std::vector<T> *vec_;
};

template <typename T, typename... Args>
PushVec(std::vector<T> *, Args &&...)->PushVec<T>;

void MakeAllStackAllocations(Compiler &compiler, ast::FnScope const *fn_scope) {
  for (auto *scope : fn_scope->descendants()) {
    if (scope != fn_scope and scope->is<ast::FnScope>()) { continue; }
    for (const auto &[key, val] : scope->decls_) {
      LOG("MakeAllStackAllocations", "%s", key);
      for (auto *decl : val) {
        if (decl->flags() &
            (ast::Declaration::f_IsConst | ast::Declaration::f_IsFnParam)) {
          LOG("MakeAllStackAllocations", "skipping constant/param decl %s",
              decl->id());
          continue;
        }

        LOG("MakeAllStackAllocations", "allocating %s", decl->id());

        compiler.context().set_addr(
            decl, compiler.builder().Alloca(compiler.type_of(decl)));
      }
    }
  }
}

}  // namespace

void MakeAllDestructions(Compiler &compiler, ast::ExecScope const *exec_scope) {
  // TODO store these in the appropriate order so we don't have to compute this?
  // Will this be faster?
  std::vector<ast::Declaration *> ordered_decls;
  LOG("MakeAllDestructions", "decls in this scope:");
  for (auto &[name, decls] : exec_scope->decls_) {
    LOG("MakeAllDestructions", "... %s", name);
    ordered_decls.insert(ordered_decls.end(), decls.begin(), decls.end());
  }

  // TODO eek, don't use line number to determine destruction order!
  absl::c_sort(ordered_decls, [](ast::Declaration *lhs, ast::Declaration *rhs) {
    return (lhs->range().begin() > rhs->range().begin());
  });

  for (auto *decl : ordered_decls) {
    type::Type t = compiler.type_of(decl);
    if (not t.get()->HasDestructor()) { continue; }
    compiler.EmitDestroy(
        type::Typed<ir::Reg>(compiler.context().addr(decl), t));
  }
}

// TODO One problem with this setup is that we don't end up calling destructors
// if we exit early, so those need to be handled externally.
void EmitIrForStatements(Compiler &compiler,
                         base::PtrSpan<ast::Node const> span) {
  ICARUS_SCOPE(ir::SetTemporaries(compiler.builder())) {
    for (auto *stmt : span) {
      LOG("EmitIrForStatements", "%s", stmt->DebugString());
      compiler.EmitValue(stmt);
      compiler.builder().FinishTemporariesWith(
          [&compiler](type::Typed<ir::Reg> r) { compiler.EmitDestroy(r); });
      LOG("EmitIrForStatements", "%p %s", compiler.builder().CurrentBlock(),
          *compiler.builder().CurrentGroup());

      if (compiler.builder().block_termination_state() !=
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

  ir::NativeFn ir_func =
      *ASSERT_NOT_NULL(compiler->context().FindNativeFn(node));

  auto &bldr = compiler->builder();
  ICARUS_SCOPE(ir::SetCurrent(ir_func, bldr)) {
    bldr.CurrentBlock() = bldr.CurrentGroup()->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      compiler->context().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*compiler, node->body_scope());

    type::Type ret_type = t->output()[0];
    if (ret_type.get()->is_big()) {
      type::Typed<ir::RegOr<ir::Addr>> typed_alloc(
          ir::RegOr<ir::Addr>(compiler->builder().GetRet(0, ret_type)),
          type::Ptr(ret_type));
      compiler->EmitMoveInit(node->body(),
                             absl::MakeConstSpan(&typed_alloc, 1));
    } else {
      compiler->builder().SetRet(
          0,
          type::Typed<ir::Value>(compiler->EmitValue(node->body()), ret_type));
    }

    bldr.FinishTemporariesWith([compiler](type::Typed<ir::Reg> r) {
      if (r.type().get()->HasDestructor()) { compiler->EmitDestroy(r); }
    });

    MakeAllDestructions(*compiler, node->body_scope());
    bldr.ReturnJump();
  }

  ir_func->work_item = nullptr;
  ir_func->WriteByteCode<interpretter::instruction_set_t>();
}

void CompleteBody(Compiler *compiler, ast::FunctionLiteral const *node,
                  type::Function const *t) {
  LOG("CompleteBody", "%s", node->DebugString());
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  ir::NativeFn ir_func =
      *ASSERT_NOT_NULL(compiler->context().FindNativeFn(node));

  auto &bldr = compiler->builder();
  ICARUS_SCOPE(ir::SetCurrent(ir_func, bldr)) {
    bldr.CurrentBlock() = bldr.CurrentGroup()->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      compiler->context().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*compiler, node->body_scope());
    if (auto outputs = node->outputs()) {
      for (size_t i = 0; i < outputs->size(); ++i) {
        auto *out_decl = (*outputs)[i]->if_as<ast::Declaration>();
        if (not out_decl) { continue; }
        type::Type out_decl_type = compiler->type_of(out_decl);
        auto alloc               = out_decl_type.get()->is_big()
                         ? bldr.GetRet(i, out_decl_type)
                         : bldr.Alloca(out_decl_type);

        compiler->context().set_addr(out_decl, alloc);
        if (out_decl->IsDefaultInitialized()) {
          compiler->EmitDefaultInit(type::Typed<ir::Reg>(alloc, out_decl_type));
        } else {
          compiler->EmitCopyAssign(
              type::Typed<ir::RegOr<ir::Addr>>(alloc, out_decl_type),
              type::Typed<ir::Value>(compiler->EmitValue(out_decl->init_val()),
                                     out_decl_type));
        }
      }
    }

    EmitIrForStatements(*compiler, node->stmts());
    MakeAllDestructions(*compiler, node->body_scope());
    bldr.ReturnJump();
  }

  ir_func->work_item = nullptr;
  ir_func->WriteByteCode<interpretter::instruction_set_t>();
}

void CompleteBody(Compiler *compiler,
                  ast::ParameterizedStructLiteral const *node) {
  NOT_YET();
}

void CompleteBody(Compiler *compiler, ast::Jump const *node) {
  LOG("CompleteBody", "Jump %s", node->DebugString());
  ir::CompiledJump &jmp = *ASSERT_NOT_NULL(compiler->context().jump(node));

  ICARUS_SCOPE(ir::SetCurrent(jmp, compiler->builder())) {
    ASSERT(compiler != nullptr);
    compiler->builder().CurrentBlock() = jmp.entry();
    // TODO arguments should be renumbered to not waste space on const
    // values
    int32_t i = 0;
    if (node->state()) {
      compiler->context().set_addr(node->state(), ir::Reg::Arg(i++));
    }
    for (auto const &param : node->params()) {
      compiler->context().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*compiler, node->body_scope());

    EmitIrForStatements(*compiler, node->stmts());

    // TODO: it seems like this will be appended after ChooseJump, which means
    // it'll never be executed.
    MakeAllDestructions(*compiler, node->body_scope());
  }
  jmp.WriteByteCode<interpretter::instruction_set_t>();
  jmp.work_item = nullptr;
}

void ProcessExecutableBody(Compiler *c, base::PtrSpan<ast::Node const> nodes,
                           ir::CompiledFn *main_fn) {
  ASSERT(nodes.size() > 0);
  ast::ModuleScope *mod_scope = &nodes.front()->scope()->as<ast::ModuleScope>();
  ICARUS_SCOPE(ir::SetCurrent(*main_fn, c->builder())) {
    MakeAllStackAllocations(*c, mod_scope);
    EmitIrForStatements(*c, nodes);
    MakeAllDestructions(*c, mod_scope);
    // TODO determine under which scenarios destructors can be skipped.

    c->builder().ReturnJump();
  }
  c->CompleteDeferredBodies();
}

ir::Value Compiler::EmitValue(ast::BlockNode const *node) {
  LOG("BlockNode", "EmitValue for block node named %s", node->name());
  ICARUS_SCOPE(PushVec(&state_.yields)) {
    EmitIrForStatements(*this, node->stmts());
    MakeAllDestructions(*this, node->body_scope());
  }
  return ir::Value();
}

}  // namespace compiler
