#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/scope/fn.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::YieldStmt const *node) {
  std::vector<std::pair<ast::Expression const *, ir::Value>> arg_vals;
  arg_vals.reserve(node->exprs().size());
  for (auto *expr : node->exprs()) {
    arg_vals.emplace_back(expr, EmitValue(expr));
  }

  // TODO: store this as an exec_scope.
  MakeAllDestructions(*this, &node->scope()->as<ast::ExecScope>());

  if (ast::Label const *lbl = node->label()) {
    auto iter = state().scope_landings.rbegin();
    for (; iter->label == lbl->value(); ++iter) {
      // TODO: Emit all destructions on this scope.
      // TODO: Call the quick-exit for this scope.
    }
    // TODO: Call `before` with arguments.
    ir::CompiledScope *compiled_scope = ir::CompiledScope::From(iter->scope);

    core::Arguments<type::QualType> yield_arg_types;
    core::Arguments<type::Typed<ir::Value>> yield_arg_typed_values;
    for (auto const &[expr, value] : arg_vals) {
      type::QualType const *qt = context().qual_type(expr);
      yield_arg_types.pos_emplace(*qt);
      yield_arg_typed_values.pos_emplace(value, qt->type());
      // TODO: Determine if you are going to support named yields.
    }

    ir::OverloadSet &exit = compiled_scope->exit();
    ir::Fn exit_fn        = exit.Lookup(yield_arg_types).value();

    type::Type t;
    if (iter->result_type.expansion_size() == 1) {
      t = iter->result_type.type();
    }
    absl::Span<type::Type const> result_types =
        iter->result_type.expansion_size() == 1
            ? absl::Span<type::Type const>(&t, 1)
            : iter->result_type.expanded();
    auto out_params = builder().OutParams(result_types);

    auto inst_iter = iter->block->instructions().begin();
    auto out_iter  = out_params.regs().begin();
    for (type::Type const &result_type : result_types) {
      // TODO: Support all types
      type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                       uint16_t, uint32_t, uint64_t, float, double>(
          result_type, [&]<typename T>() {
            ASSERT(inst_iter != iter->block->instructions().end());
            ASSERT(static_cast<bool>(*inst_iter) == true);
            ASSERT(out_iter != out_params.regs().end());
            auto &phi = inst_iter->template as<ir::PhiInstruction<T>>();
            phi.blocks.push_back(builder().CurrentBlock());
            phi.values.push_back(*out_iter);
            ++inst_iter;
            ++out_iter;
          });
    }

    builder().Call(
        exit_fn, exit_fn.type(),
        PrepareCallArguments(compiled_scope->state_type(),
                             exit_fn.type()->params(), yield_arg_typed_values),
        std::move(out_params));

    builder().UncondJump(iter->block);

    // TODO: Wire up phi node.
    builder().block_termination_state() =
        ir::Builder::BlockTerminationState::kLabeledYield;
  } else {
    builder().block_termination_state() =
        ir::Builder::BlockTerminationState::kYield;
  }

  return ir::Value();
}

}  // namespace compiler
