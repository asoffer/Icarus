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

void CompleteBody(Compiler &compiler, ast::ShortFunctionLiteral const *node,
                  type::Function const *t) {
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

    type::Type ret_type = t->output()[0];
    if (ret_type.get()->is_big()) {
      type::Typed<ir::RegOr<ir::Addr>> typed_alloc(
          ir::RegOr<ir::Addr>(compiler.builder().GetRet(0, ret_type)),
          type::Ptr(ret_type));
      compiler.EmitMoveInit(node->body(), absl::MakeConstSpan(&typed_alloc, 1));
    } else {
      compiler.builder().SetRet(
          0,
          type::Typed<ir::Value>(compiler.EmitValue(node->body()), ret_type));
    }

    bldr.FinishTemporariesWith([&compiler](type::Typed<ir::Reg> r) {
      if (r.type().get()->HasDestructor()) { compiler.EmitDestroy(r); }
    });

    MakeAllDestructions(compiler, node->body_scope());
    bldr.ReturnJump();
  }

  ir_func->work_item = nullptr;
  ir_func->WriteByteCode<interpreter::instruction_set_t>();
}

}  // namespace

ir::Value Compiler::EmitValue(ast::ShortFunctionLiteral const *node) {
  if (node->is_generic()) {
    auto gen_fn = ir::GenericFn(
        [c = Compiler(resources()),
         node](core::Arguments<type::Typed<ir::Value>> const &args) mutable
        -> ir::NativeFn {
          return MakeConcreteFromGeneric<ast::ShortFunctionLiteral,
                                         CompleteBody>(c, node, args);
        });
    return ir::Value(gen_fn);
  }

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
