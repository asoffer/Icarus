#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/value/char.h"
#include "type/cast.h"

namespace compiler {
namespace {
ir::RegOr<bool> EmitPair(Compiler &c, ast::ComparisonOperator const *node,
                         size_t index,
                         type::Typed<ir::PartialResultBuffer> const &lhs,
                         type::Typed<ir::PartialResultBuffer> const &rhs) {
  auto &bldr = c.builder();
  auto op    = node->ops()[index];
  if (lhs.type().is<type::Array>() and rhs.type().is<type::Array>()) {
    NOT_YET();
  } else if (lhs.type().is<type::Struct>() or rhs.type().is<type::Struct>()) {
    NOT_YET();
  } else if (lhs.type().is<type::Flags>() and lhs.type() == rhs.type()) {
    using underlying_type = type::Flags::underlying_type;
    auto lhs_value        = lhs->get<underlying_type>(0);
    auto rhs_value        = rhs->get<underlying_type>(0);
    switch (op) {
      case frontend::Operator::Gt:
        std::swap(lhs_value, rhs_value);
        [[fallthrough]];
      case frontend::Operator::Lt: {
        auto not_equal =
            c.current_block()->Append(ir::NeInstruction<underlying_type>{
                .lhs    = lhs_value,
                .rhs    = rhs_value,
                .result = bldr.CurrentGroup()->Reserve()});
        auto mask = c.current_block()->Append(
            type::OrFlagsInstruction{.lhs    = lhs_value,
                                     .rhs    = rhs_value,
                                     .result = bldr.CurrentGroup()->Reserve()});
        auto less_or_equal_mask =
            c.current_block()->Append(ir::LeInstruction<underlying_type>{
                .lhs    = mask,
                .rhs    = rhs_value,
                .result = bldr.CurrentGroup()->Reserve()});
        return c.current_block()->Append(
            ir::AndInstruction{.lhs    = not_equal,
                               .rhs    = less_or_equal_mask,
                               .result = bldr.CurrentGroup()->Reserve()});
      }
      case frontend::Operator::Ge:
        std::swap(lhs_value, rhs_value);
        [[fallthrough]];
      case frontend::Operator::Le: {
        auto mask = c.current_block()->Append(
            type::OrFlagsInstruction{.lhs    = lhs_value,
                                     .rhs    = rhs_value,
                                     .result = bldr.CurrentGroup()->Reserve()});
        return c.current_block()->Append(ir::LeInstruction<underlying_type>{
            .lhs    = mask,
            .rhs    = rhs_value,
            .result = bldr.CurrentGroup()->Reserve()});
      }
      case frontend::Operator::Eq:
        return bldr.CurrentBlock()->Append(ir::EqInstruction<underlying_type>{
            .lhs    = lhs_value,
            .rhs    = rhs_value,
            .result = bldr.CurrentGroup()->Reserve()});
      case frontend::Operator::Ne:
        return bldr.CurrentBlock()->Append(ir::NeInstruction<underlying_type>{
            .lhs    = lhs_value,
            .rhs    = rhs_value,
            .result = bldr.CurrentGroup()->Reserve()});
      default: UNREACHABLE();
    }

  } else {
    type::Type t = type::Meet(lhs.type(), rhs.type());
    ir::PartialResultBuffer lhs_buffer = *lhs, rhs_buffer = *rhs;
    EmitCast(bldr, t, lhs.type(), lhs_buffer);
    EmitCast(bldr, t, rhs.type(), rhs_buffer);
    switch (op) {
      case frontend::Operator::Gt:
        std::swap(lhs_buffer, rhs_buffer);
        [[fallthrough]];
      case frontend::Operator::Lt:
        return ApplyTypes<ir::Char, ir::Integer, int8_t, int16_t, int32_t,
                          int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float,
                          double, ir::addr_t>(
            t, [&]<typename T>() -> ir::RegOr<bool> {
              return bldr.CurrentBlock()->Append(ir::LtInstruction<T>{
                  .lhs    = lhs_buffer.back().get<T>(),
                  .rhs    = rhs_buffer.back().get<T>(),
                  .result = bldr.CurrentGroup()->Reserve()});
            });

      case frontend::Operator::Ge:
        std::swap(lhs_buffer, rhs_buffer);
        [[fallthrough]];
      case frontend::Operator::Le:
        return ApplyTypes<ir::Char, ir::Integer, int8_t, int16_t, int32_t,
                          int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float,
                          double, ir::addr_t>(
            t, [&]<typename T>() -> ir::RegOr<bool> {
              return bldr.CurrentBlock()->Append(ir::LeInstruction<T>{
                  .lhs    = lhs_buffer.back().get<T>(),
                  .rhs    = rhs_buffer.back().get<T>(),
                  .result = bldr.CurrentGroup()->Reserve()});
            });
      case frontend::Operator::Eq:
        return ApplyTypes<bool, ir::Integer, ir::Char, int8_t, int16_t, int32_t,
                          int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float,
                          double, type::Type, ir::addr_t>(t, [&]<typename T>() {
          return bldr.CurrentBlock()->Append(
              ir::EqInstruction<T>{.lhs    = lhs->back().get<T>(),
                                   .rhs    = rhs->back().get<T>(),
                                   .result = bldr.CurrentGroup()->Reserve()});
        });
      case frontend::Operator::Ne:
        return ApplyTypes<bool, ir::Integer, ir::Char, int8_t, int16_t, int32_t,
                          int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float,
                          double, type::Type, ir::addr_t>(t, [&]<typename T>() {
          return bldr.CurrentBlock()->Append(
              ir::NeInstruction<T>{.lhs    = lhs->back().get<T>(),
                                   .rhs    = rhs->back().get<T>(),
                                   .result = bldr.CurrentGroup()->Reserve()});
        });
      default: UNREACHABLE();
    }
  }
}

}  // namespace

void Compiler::EmitToBuffer(ast::ComparisonOperator const *node,
                            ir::PartialResultBuffer &out) {
  if (node->ops().size() == 1) {
    ir::PartialResultBuffer lhs_buffer, rhs_buffer;
    EmitToBuffer(node->exprs()[0], lhs_buffer);
    EmitToBuffer(node->exprs()[1], rhs_buffer);
    type::Type lhs_type = context().qual_types(node->exprs()[0])[0].type();
    type::Type rhs_type = context().qual_types(node->exprs()[1])[0].type();
    out.append(EmitPair(*this, node, 0, type::Typed(lhs_buffer, lhs_type),
                        type::Typed(rhs_buffer, rhs_type)));
    return;
  }

  std::vector<ir::BasicBlock const *> phi_blocks;
  std::vector<ir::RegOr<bool>> phi_values;

  ir::PartialResultBuffer lhs_buffer, rhs_buffer;
  EmitToBuffer(node->exprs()[0], lhs_buffer);
  type::Type lhs_type = context().qual_types(node->exprs()[0])[0].type();

  auto *land_block = builder().CurrentGroup()->AppendBlock();
  for (size_t i = 0; i + 1 < node->ops().size(); ++i) {
    rhs_buffer.clear();
    EmitToBuffer(node->exprs()[i], rhs_buffer);
    type::Type rhs_type = context().qual_types(node->exprs()[i])[0].type();

    auto cmp = EmitPair(*this, node, i, type::Typed(lhs_buffer, lhs_type),
                        type::Typed(rhs_buffer, rhs_type));

    phi_blocks.push_back(builder().CurrentBlock());
    phi_values.push_back(false);
    auto *next_block = builder().CurrentGroup()->AppendBlock();
    current_block()->set_jump(ir::JumpCmd::Cond(cmp, next_block, land_block));
    builder().CurrentBlock() = next_block;
    lhs_buffer               = std::move(rhs_buffer);
    lhs_type                 = rhs_type;
  }

  // Once more for the last element, but don't do a conditional jump.
  EmitToBuffer(node->exprs()[0], rhs_buffer);
  type::Type rhs_type = context().qual_types(node->exprs().back())[0].type();

  phi_blocks.push_back(builder().CurrentBlock());
  phi_values.push_back(EmitPair(*this, node, node->exprs().size() - 2,
                                type::Typed(lhs_buffer, lhs_type),
                                type::Typed(rhs_buffer, rhs_type)));

  current_block()->set_jump(ir::JumpCmd::Uncond(land_block));

  builder().CurrentBlock() = land_block;

  ir::PhiInstruction<bool> phi(std::move(phi_blocks), std::move(phi_values));
  phi.result = builder().CurrentGroup()->Reserve();
  out.append(current_block()->Append(std::move(phi)));
}

void Compiler::EmitCopyAssign(
    ast::ComparisonOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyAssign(to[0], type::Typed(buffer[0], type::Bool));
}

void Compiler::EmitMoveAssign(
    ast::ComparisonOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveAssign(to[0], type::Typed(buffer[0], type::Bool));
}

void Compiler::EmitCopyInit(
    ast::ComparisonOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyAssign(to[0], type::Typed(buffer[0], type::Bool));
}

void Compiler::EmitMoveInit(
    ast::ComparisonOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveAssign(to[0], type::Typed(buffer[0], type::Bool));
}

}  // namespace compiler
