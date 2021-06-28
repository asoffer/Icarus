#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/char.h"

namespace compiler {
namespace {
ir::RegOr<bool> EmitPair(Compiler &compiler,
                         ast::ComparisonOperator const *node, size_t index,
                         type::Typed<ir::Value> lhs,
                         type::Typed<ir::Value> rhs) {
  auto &bldr = compiler.builder();
  auto op    = node->ops()[index];
  if (lhs.type().is<type::Array>() and rhs.type().is<type::Array>()) {
    NOT_YET();
  } else if (lhs.type().is<type::Struct>() or rhs.type().is<type::Struct>()) {
    NOT_YET();
  } else if (lhs.type().is<type::Flags>() and lhs.type() == rhs.type()) {
    using underlying_type = type::Flags::underlying_type;
    auto lhs_value        = lhs->get<ir::RegOr<underlying_type>>();
    auto rhs_value        = rhs->get<ir::RegOr<underlying_type>>();
    switch (op) {
      case frontend::Operator::Lt:
        return compiler.current_block()->Append(ir::AndInstruction{
            .lhs = bldr.Ne(lhs_value, rhs_value),
            .rhs = bldr.Le(
                ir::RegOr<underlying_type>(
                    compiler.current_block()->Append(type::OrFlagsInstruction{
                        .lhs    = lhs_value,
                        .rhs    = rhs_value,
                        .result = bldr.CurrentGroup()->Reserve()})),
                rhs_value),
            .result = bldr.CurrentGroup()->Reserve()});
      case frontend::Operator::Le:
        return bldr.Le(
            ir::RegOr<underlying_type>(
                compiler.current_block()->Append(type::OrFlagsInstruction{
                    .lhs    = lhs_value,
                    .rhs    = rhs_value,
                    .result = bldr.CurrentGroup()->Reserve()})),
            rhs_value);
      case frontend::Operator::Eq: return bldr.Eq(lhs_value, rhs_value);
      case frontend::Operator::Ne: return bldr.Ne(lhs_value, rhs_value);
      case frontend::Operator::Ge:
        return bldr.Le(
            ir::RegOr<underlying_type>(
                compiler.current_block()->Append(type::OrFlagsInstruction{
                    .lhs    = lhs_value,
                    .rhs    = rhs_value,
                    .result = bldr.CurrentGroup()->Reserve()})),
            lhs_value);
      case frontend::Operator::Gt:
        return compiler.current_block()->Append(ir::AndInstruction{
            .lhs = bldr.Ne(lhs_value, rhs_value),
            .rhs = bldr.Le(
                ir::RegOr<underlying_type>(
                    compiler.current_block()->Append(type::OrFlagsInstruction{
                        .lhs    = lhs_value,
                        .rhs    = rhs_value,
                        .result = bldr.CurrentGroup()->Reserve()})),
                lhs_value),
            .result = bldr.CurrentGroup()->Reserve()});
      default: UNREACHABLE();
    }

  } else {
    type::Type t = type::Meet(lhs.type(), rhs.type());
    switch (op) {
      case frontend::Operator::Lt:
        return ApplyTypes<ir::Char, ir::Integer, int8_t, int16_t, int32_t,
                          int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float,
                          double, ir::addr_t>(t, [&]<typename T>() {
          return bldr.Lt(bldr.CastTo<T>(lhs), bldr.CastTo<T>(rhs));
        });
      case frontend::Operator::Le:
        return ApplyTypes<ir::Char, ir::Integer, int8_t, int16_t, int32_t,
                          int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float,
                          double, ir::addr_t>(t, [&]<typename T>() {
          return bldr.Le(bldr.CastTo<T>(lhs), bldr.CastTo<T>(rhs));
        });
      case frontend::Operator::Eq:
        if (t == type::Block) {
          auto val1 = lhs->get<ir::RegOr<ir::Block>>();
          auto val2 = rhs->get<ir::RegOr<ir::Block>>();
          if (not val1.is_reg() and not val2.is_reg()) {
            return val1.value() == val2.value();
          }
        }
        return ApplyTypes<bool, ir::Integer, ir::Char, int8_t, int16_t, int32_t,
                          int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float,
                          double, type::Type, ir::addr_t>(t, [&]<typename T>() {
          return bldr.Eq(bldr.CastTo<T>(lhs), bldr.CastTo<T>(rhs));
        });
      case frontend::Operator::Ne:
        if (t == type::Block) {
          auto val1 = lhs->get<ir::RegOr<ir::Block>>();
          auto val2 = rhs->get<ir::RegOr<ir::Block>>();
          if (not val1.is_reg() and not val2.is_reg()) {
            return val1.value() == val2.value();
          }
        }
        return ApplyTypes<bool, ir::Integer, ir::Char, int8_t, int16_t, int32_t,
                          int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float,
                          double, type::Type, ir::addr_t>(t, [&]<typename T>() {
          return bldr.Ne(bldr.CastTo<T>(lhs), bldr.CastTo<T>(rhs));
        });
      case frontend::Operator::Ge:
        return ApplyTypes<ir::Char, ir::Integer, int8_t, int16_t, int32_t,
                          int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float,
                          double, ir::addr_t>(t, [&]<typename T>() {
          return bldr.Le(bldr.CastTo<T>(rhs), bldr.CastTo<T>(lhs));
        });
      case frontend::Operator::Gt:
        return ApplyTypes<ir::Char, ir::Integer, int8_t, int16_t, int32_t,
                          int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float,
                          double, ir::addr_t>(t, [&]<typename T>() {
          return bldr.Lt(bldr.CastTo<T>(rhs), bldr.CastTo<T>(lhs));
        });
        // TODO case frontend::Operator::And: cmp = *lhs; break;
      default: UNREACHABLE();
    }
  }
}

type::Typed<ir::Value> EmitTypedValue(Compiler &c,
                                      ast::Expression const *expr) {
  return type::Typed<ir::Value>(c.EmitValue(expr),
                                c.context().qual_types(expr)[0].type());
}

}  // namespace

ir::Value Compiler::EmitValue(ast::ComparisonOperator const *node) {
  if (node->ops().size() == 1) {
    return ir::Value(EmitPair(*this, node, 0,
                              EmitTypedValue(*this, node->exprs()[0]),
                              EmitTypedValue(*this, node->exprs()[1])));
  }

  std::vector<ir::BasicBlock const *> phi_blocks;
  std::vector<ir::RegOr<bool>> phi_values;
  auto lhs         = EmitTypedValue(*this, node->exprs().front());
  auto *land_block = builder().AddBlock();
  for (size_t i = 0; i + 1 < node->ops().size(); ++i) {
    auto rhs = EmitTypedValue(*this, node->exprs()[i + 1]);
    auto cmp = EmitPair(*this, node, i, lhs, rhs);

    phi_blocks.push_back(builder().CurrentBlock());
    phi_values.push_back(false);
    auto *next_block = builder().AddBlock();
    builder().CondJump(cmp, next_block, land_block);
    builder().CurrentBlock() = next_block;
    lhs                      = std::move(rhs);
  }

  // Once more for the last element, but don't do a conditional jump.
  auto rhs = EmitTypedValue(*this, node->exprs().back());
  phi_blocks.push_back(builder().CurrentBlock());
  phi_values.push_back(
      EmitPair(*this, node, node->exprs().size() - 2, lhs, rhs));
  builder().UncondJump(land_block);

  builder().CurrentBlock() = land_block;

  return ir::Value(
      builder().Phi<bool>(std::move(phi_blocks), std::move(phi_values)));
}

}  // namespace compiler
