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
  // Note: yields do not allow passing named arguments, so there's no need for a
  // full `core::Arguments` here. We can proceed direclty to a vector of the
  // positional arguments.

  core::Arguments<type::QualType> yield_arg_types;
  std::vector<type::Typed<ir::Value>> constant_arguments;
  for (auto const *expr : node->exprs()) {
    auto qt = *ASSERT_NOT_NULL(context().qual_type(expr));
    yield_arg_types.pos_emplace(qt);
    if (qt.constant()) {
      ir::Value result = EvaluateOrDiagnose(
          type::Typed<ast::Expression const *>(expr, qt.type()));
      if (result.empty()) { NOT_YET(); }
      constant_arguments.emplace_back(result, qt.type());
    } else {
      constant_arguments.emplace_back(ir::Value(), qt.type());
    }
  }

  if (ast::Label const *lbl = node->label()) {
    auto iter = state().scope_landings.rbegin();
    for (; iter->label == lbl->value(); ++iter) {
      // TODO: Emit all destructions on this scope.
      // TODO: Call the quick-exit for this scope.
    }

    ir::OverloadSet &exit = ir::CompiledScope::From(iter->scope)->exit();
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

    std::vector<ir::Value> prepared_arguments;
    prepared_arguments.reserve(yield_arg_types.size());
    size_t i = 0;
    for (auto const *expr : node->exprs()) {
      prepared_arguments.push_back(
          PrepareArgument(*this, *constant_arguments[i], expr,
                          exit_fn.type()->params()[i].value));
      ++i;
    }

    builder().Call(exit_fn, exit_fn.type(), std::move(prepared_arguments),
                   std::move(out_params));

    builder().UncondJump(iter->block);

    // TODO: Wire up phi node.
    builder().block_termination_state() =
        ir::Builder::BlockTerminationState::kLabeledYield;
  } else {
    builder().block_termination_state() =
        ir::Builder::BlockTerminationState::kYield;
  }

  // TODO: store this as an exec_scope.
  MakeAllDestructions(*this, &node->scope()->as<ast::ExecScope>());

  return ir::Value();
}

}  // namespace compiler
