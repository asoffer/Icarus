#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "core/arguments.h"
#include "ir/value/value.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::ShortFunctionLiteral const *node) {
  if (node->is_generic()) {
    auto gen_fn = ir::GenericFn(
        [c = Compiler(resources()),
         node](core::Arguments<type::Typed<ir::Value>> const &args) mutable
        -> ir::NativeFn {
          auto find_subcontext_result = c.FindInstantiation(node, args);
          auto &context               = find_subcontext_result.context;

          auto [f, inserted] = context.add_func(node);
          Compiler compiler({
              .data                = context,
              .diagnostic_consumer = c.diag(),
              .importer            = c.importer(),
          });
          if (inserted) {
            compiler.Enqueue({.kind = WorkItem::Kind::EmitShortFunctionBody,
                              .node = node,
                              .resources = compiler.resources()});
          }

          compiler.CompleteWorkQueue();
          compiler.CompleteDeferredBodies();

          return f;
        });
    return ir::Value(gen_fn);
  }

  auto [f, inserted] = context().add_func(node);
  if (inserted) {
    Enqueue({.kind      = WorkItem::Kind::EmitShortFunctionBody,
             .node      = node,
             .resources = resources_});
  }
  return ir::Value(ir::Fn{f});
}

WorkItem::Result Compiler::EmitShortFunctionBody(
    ast::ShortFunctionLiteral const *node) {
  ir::NativeFn ir_func =
      *ASSERT_NOT_NULL(context().FindNativeFn(node));

  ICARUS_SCOPE(ir::SetCurrent(ir_func, builder())) {
    builder().CurrentBlock() = builder().CurrentGroup()->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      context().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*this, node->body_scope());

    type::Type ret_type = ir_func.type()->output()[0];
    if (ret_type.get()->is_big()) {
      type::Typed<ir::RegOr<ir::Addr>> typed_alloc(
          ir::RegOr<ir::Addr>(builder().GetRet(0, ret_type)),
          type::Ptr(ret_type));
      EmitMoveInit(node->body(), absl::MakeConstSpan(&typed_alloc, 1));
    } else {
      builder().SetRet(
          0, type::Typed<ir::Value>(EmitValue(node->body()), ret_type));
    }

    builder().FinishTemporariesWith([this](type::Typed<ir::Reg> r) {
      if (r.type().get()->HasDestructor()) { EmitDestroy(r); }
    });

    MakeAllDestructions(*this, node->body_scope());
    builder().ReturnJump();
  }

  ir_func->WriteByteCode<interpreter::instruction_set_t>();
  return WorkItem::Result::Success;
}

}  // namespace compiler
