#include "ast/ast.h"
#include "base/move_func.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "core/arguments.h"
#include "ir/value/value.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

void CompleteBody(Compiler &compiler, ast::FunctionLiteral const *node,
                  type::Function const *t) {
  LOG("CompleteBody", "%s", node->DebugString());
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  ir::NativeFn ir_func =
      *ASSERT_NOT_NULL(compiler.context().FindNativeFn(node));

  auto &bldr = compiler.builder();
  ICARUS_SCOPE(ir::SetCurrent(ir_func, bldr)) {
    bldr.CurrentBlock() = bldr.CurrentGroup()->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      compiler.context().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(compiler, node->body_scope());
    if (auto outputs = node->outputs()) {
      for (size_t i = 0; i < outputs->size(); ++i) {
        auto *out_decl = (*outputs)[i]->if_as<ast::Declaration>();
        if (not out_decl) { continue; }
        type::Type out_decl_type =
            compiler.context().qual_type(out_decl)->type();
        auto alloc = out_decl_type.get()->is_big()
                         ? bldr.GetRet(i, out_decl_type)
                         : bldr.Alloca(out_decl_type);

        compiler.context().set_addr(out_decl, alloc);
        if (out_decl->IsDefaultInitialized()) {
          compiler.EmitDefaultInit(type::Typed<ir::Reg>(alloc, out_decl_type));
        } else {
          compiler.EmitCopyAssign(
              type::Typed<ir::RegOr<ir::Addr>>(alloc, out_decl_type),
              type::Typed<ir::Value>(compiler.EmitValue(out_decl->init_val()),
                                     out_decl_type));
        }
      }
    }

    EmitIrForStatements(compiler, node->stmts());
    MakeAllDestructions(compiler, node->body_scope());
    bldr.ReturnJump();
  }

  ir_func->work_item = nullptr;
  ir_func->WriteByteCode<interpreter::instruction_set_t>();
}

}  // namespace

ir::Value Compiler::EmitValue(ast::FunctionLiteral const *node) {
  if (node->is_generic()) {
    auto gen_fn = ir::GenericFn(
        [c = this->WithPersistent(),
         node](core::Arguments<type::Typed<ir::Value>> const &args) mutable
        -> ir::NativeFn {
          return MakeConcreteFromGeneric<ast::FunctionLiteral, CompleteBody>(
              c, node, args);
        });
    return ir::Value(gen_fn);
  }

  // TODO: Check the result of body verification.
  // TODO: Check for whether or not we actually need to do the verification is
  // handled inside VerifyBody, at least for now.
  VerifyBody(node);

  // TODO Use correct constants
  auto [f, inserted] = context().add_func(node);
  if (inserted) {
    f->work_item =
        state_.deferred_work
            .emplace_back(std::make_unique<base::move_func<void()>>(
                [c = Compiler(resources_), node, t = f.type()]() mutable {
                  CompleteBody(c, node, t);
                }))
            .get();
  }
  return ir::Value(ir::Fn{f});
}

}  // namespace compiler
