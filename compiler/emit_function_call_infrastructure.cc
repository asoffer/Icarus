#include "compiler/emit_function_call_infrastructure.h"

#include <vector>

#include "base/debug.h"
#include "ir/builder.h"

namespace compiler {

void MakeAllStackAllocations(Compiler *compiler, ast::FnScope const *fn_scope) {
  for (auto *scope : fn_scope->descendants()) {
    if (scope != fn_scope and scope->is<ast::FnScope>()) { continue; }
    for (const auto &[key, val] : scope->decls_) {
      DEBUG_LOG("MakeAllStackAllocations")(key);
      for (auto *decl : val) {
        if (decl->flags() &
            (ast::Declaration::f_IsConst | ast::Declaration::f_IsFnParam)) {
          continue;
        }

        compiler->set_addr(decl,
                           compiler->builder().Alloca(compiler->type_of(decl)));
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
    return (lhs->span.begin() > rhs->span.begin());
  });

  for (auto *decl : ordered_decls) {
    auto *t = ASSERT_NOT_NULL(compiler->type_of(decl));
    if (not t->HasDestructor()) { continue; }
    compiler->Visit(t, compiler->addr(decl), EmitDestroyTag{});
  }
}

// TODO One problem with this setup is that we don't end up calling destructors
// if we exit early, so those need to be handled externally.
void EmitIrForStatements(Compiler *compiler,
                         base::PtrSpan<ast::Node const> span) {
  ICARUS_SCOPE(ir::SetTemporaries(compiler->builder())) {
    for (auto *stmt : span) {
      DEBUG_LOG("EmitIrForStatements")(stmt->DebugString());
      compiler->Visit(stmt, EmitValueTag{});
      compiler->builder().FinishTemporariesWith(
          [compiler](type::Typed<ir::Reg> r) {
            compiler->Visit(r.type(), r.get(), EmitDestroyTag{});
          });
      if (not compiler->builder().more_stmts_allowed()) { break; };
    }
  }
}

void CompleteBody(Compiler *compiler, ast::FunctionLiteral const *node) {
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  auto *t = compiler->type_of(node);

  ir::CompiledFn *&ir_func = compiler->data_.ir_funcs_[node];

  ICARUS_SCOPE(ir::SetCurrent(ir_func)) {
    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      compiler->set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(compiler, node->body_scope());
    if (auto outputs = node->outputs()) {
      for (size_t i = 0; i < outputs->size(); ++i) {
        auto *out_decl = (*outputs)[i]->if_as<ast::Declaration>();
        if (not out_decl) { continue; }
        auto *out_decl_type = ASSERT_NOT_NULL(compiler->type_of(out_decl));
        auto alloc          = out_decl_type->is_big()
                         ? ir::GetRet(i, out_decl_type)
                         : compiler->builder().Alloca(out_decl_type);

        compiler->set_addr(out_decl, alloc);
        if (out_decl->IsDefaultInitialized()) {
          compiler->Visit(out_decl_type, alloc, EmitDefaultInitTag{});
        } else {
          compiler->Visit(
              out_decl_type, alloc,
              type::Typed{compiler->Visit(out_decl->init_val(), EmitValueTag{}),
                          out_decl_type},
              EmitCopyAssignTag{});
        }
      }
    }

    EmitIrForStatements(compiler, node->stmts());
    MakeAllDestructions(compiler, node->body_scope());

    if (t->as<type::Function>().output.empty()) {
      // TODO even this is wrong. Figure out the right jumping strategy
      // between here and where you call SetReturn
      compiler->builder().ReturnJump();
    }
  }
  ir_func->WriteByteCode();
  ir_func->work_item = nullptr;
}

void CompleteBody(Compiler *compiler,
                  ast::ParameterizedStructLiteral const *node) {
  NOT_YET();
  //   ir::CompiledFn *&ir_func = data_.ir_funcs_[node];
  //   for (size_t i = 0; i < node->params().size(); ++i) {
  //     set_addr(&node->params()[i], ir::Reg::Arg(i));
  //   }
  //
  //   ICARUS_SCOPE(ir::SetCurrent(ir_func)) {
  //     ir::GetBuilder().CurrentBlock() = ir_func->entry();
  //     auto cache_slot_addr    = ir::ArgumentCache(node);
  //     auto cache_slot         = ir::Load<type::Type const
  //     *>(cache_slot_addr);
  //
  //     auto land_block         = builder().AddBlock();
  //     ir::GetBuilder().CurrentBlock() = ir::EarlyExitOn<false>(
  //         land_block,
  //         builder().Eq(cache_slot, static_cast<type::Type const
  //         *>(nullptr)));
  //     auto ctx_reg    = ir::CreateContext(module());
  //     auto struct_reg = ir::CreateStruct(node->scope_, node);
  //
  //     // TODO why isn't implicit TypedRegister -> RegOr cast working on
  //     // either of these? On the first it's clear because we don't even
  //     return
  //     a
  //     // typedRegister, but this is a note to remind you to make that work.
  //     On the
  //     // second... I don't know.
  //     ir::Store(static_cast<ir::RegOr<type::Type const *>>(struct_reg),
  //               cache_slot_addr);
  //     for (auto &arg : node->params()) {  // TODO const-ref
  //       ir::AddBoundConstant(ctx_reg, &arg, addr(&arg));
  //     }
  //
  //     for (auto &field : node->fields_) {  // TODO const-ref
  //       ir::VerifyType(&field, ctx_reg);
  //
  //       // TODO exit early if verifytype fails.
  //
  //       auto type_reg = ir::EvaluateAsType(field.type_expr(), ctx_reg);
  //
  //       ir::CreateStructField(struct_reg, type_reg);
  //       ir::SetStructFieldName(struct_reg, field.id());
  //
  //       // for (auto const &hashtag : field.hashtags_) {
  //       //   ir::AddHashtagToField(struct_reg, hashtag);
  //       // }
  //     }
  //
  //     // for (auto hashtag : node->hashtags_) {
  //     //   ir::AddHashtagToStruct(struct_reg, hashtag);
  //     // }
  //
  //     ir::RegOr<type::Type const *> result = ir::FinalizeStruct(struct_reg);
  //     ir::DestroyContext(ctx_reg);
  //
  //     // Exit path from creating a new struct.
  //     ir::SetRet(0, static_cast<ir::RegOr<type::Type const *>>(result));
  //     ir::Store(static_cast<ir::RegOr<type::Type const *>>(result),
  //               cache_slot_addr);
  //     compiler->builder().ReturnJump();
  //
  //     // Exit path from finding the cache
  //     ir::GetBuilder().CurrentBlock() = land_block;
  //     ir::SetRet(0, static_cast<ir::RegOr<type::Type const *>>(cache_slot));
  //     compiler->builder().ReturnJump();
  //   }
}

void CompleteBody(Compiler *compiler, ast::Jump const *node) {
  ir::Jump *jmp = ASSERT_NOT_NULL(compiler->data_.jump(node));

  ICARUS_SCOPE(ir::SetCurrent(jmp, &compiler->builder())) {
    ASSERT(compiler != nullptr);
    compiler->builder().CurrentBlock() = jmp->entry();
    // TODO arguments should be renumbered to not waste space on const
    // values
    int32_t i = 0;
    for (auto const &param : node->params()) {
      compiler->set_addr(param.value.get(), ir::Reg::Arg(i++));
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
  ast::ModuleScope *mod_scope = &nodes.front()->scope_->as<ast::ModuleScope>();
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
