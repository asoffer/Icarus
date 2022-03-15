#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/compiler_common.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/emit/initialize.h"
#include "ir/value/char.h"
#include "type/cast.h"

namespace compiler {
namespace {
ir::RegOr<bool> EmitPair(Compiler &c, ast::ComparisonOperator const *node,
                         frontend::Operator op,
                         type::Typed<ir::PartialResultBuffer> const &lhs,
                         type::Typed<ir::PartialResultBuffer> const &rhs) {
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
                .result = c.current().subroutine->Reserve()});
        auto mask = c.current_block()->Append(
            type::OrFlagsInstruction{.lhs    = lhs_value,
                                     .rhs    = rhs_value,
                                     .result = c.current().subroutine->Reserve()});
        auto less_or_equal_mask =
            c.current_block()->Append(ir::LeInstruction<underlying_type>{
                .lhs    = mask,
                .rhs    = rhs_value,
                .result = c.current().subroutine->Reserve()});
        return c.current_block()->Append(
            ir::AndInstruction{.lhs    = not_equal,
                               .rhs    = less_or_equal_mask,
                               .result = c.current().subroutine->Reserve()});
      }
      case frontend::Operator::Ge:
        std::swap(lhs_value, rhs_value);
        [[fallthrough]];
      case frontend::Operator::Le: {
        auto mask = c.current_block()->Append(
            type::OrFlagsInstruction{.lhs    = lhs_value,
                                     .rhs    = rhs_value,
                                     .result = c.current().subroutine->Reserve()});
        return c.current_block()->Append(ir::LeInstruction<underlying_type>{
            .lhs    = mask,
            .rhs    = rhs_value,
            .result = c.current().subroutine->Reserve()});
      }
      case frontend::Operator::Eq:
        return c.current_block()->Append(ir::EqInstruction<underlying_type>{
            .lhs    = lhs_value,
            .rhs    = rhs_value,
            .result = c.current().subroutine->Reserve()});
      case frontend::Operator::Ne:
        return c.current_block()->Append(ir::NeInstruction<underlying_type>{
            .lhs    = lhs_value,
            .rhs    = rhs_value,
            .result = c.current().subroutine->Reserve()});
      default: UNREACHABLE();
    }

  } else {
    type::Type t                       = type::Meet(lhs.type(), rhs.type());
    ir::PartialResultBuffer lhs_buffer = std::move(*lhs);
    ir::PartialResultBuffer rhs_buffer = std::move(*rhs);
    EmitCast(c, lhs.type(), t, lhs_buffer);
    EmitCast(c, rhs.type(), t, rhs_buffer);

    switch (op) {
      case frontend::Operator::Gt:
        std::swap(lhs_buffer, rhs_buffer);
        [[fallthrough]];
      case frontend::Operator::Lt:
        return ApplyTypes<ir::Char, int8_t, int16_t, int32_t, int64_t, uint8_t,
                          uint16_t, uint32_t, uint64_t, float, double,
                          ir::addr_t>(t, [&]<typename T>() -> ir::RegOr<bool> {
          return c.current_block()->Append(ir::LtInstruction<T>{
              .lhs    = lhs_buffer.back().get<T>(),
              .rhs    = rhs_buffer.back().get<T>(),
              .result = c.current().subroutine->Reserve()});
        });

      case frontend::Operator::Ge:
        std::swap(lhs_buffer, rhs_buffer);
        [[fallthrough]];
      case frontend::Operator::Le:
        return ApplyTypes<ir::Char, int8_t, int16_t, int32_t, int64_t, uint8_t,
                          uint16_t, uint32_t, uint64_t, float, double,
                          ir::addr_t>(t, [&]<typename T>() -> ir::RegOr<bool> {
          return c.current_block()->Append(ir::LeInstruction<T>{
              .lhs    = lhs_buffer.back().get<T>(),
              .rhs    = rhs_buffer.back().get<T>(),
              .result = c.current().subroutine->Reserve()});
        });
      case frontend::Operator::Eq:
        return ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t,
                          uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                          type::Type, ir::addr_t>(t, [&]<typename T>() {
          return c.current_block()->Append(ir::EqInstruction<T>{
              .lhs    = lhs_buffer.back().get<T>(),
              .rhs    = rhs_buffer.back().get<T>(),
              .result = c.current().subroutine->Reserve()});
        });
      case frontend::Operator::Ne:
        return ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t,
                          uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                          type::Type, ir::addr_t>(t, [&]<typename T>() {
          return c.current_block()->Append(ir::NeInstruction<T>{
              .lhs    = lhs_buffer.back().get<T>(),
              .rhs    = rhs_buffer.back().get<T>(),
              .result = c.current().subroutine->Reserve()});
        });
      default: UNREACHABLE();
    }
  }
}

}  // namespace

void Compiler::EmitToBuffer(ast::ComparisonOperator const *node,
                            ir::PartialResultBuffer &out) {
  ir::PartialResultBuffer lhs_buffer, rhs_buffer;
  EmitToBuffer(node->exprs()[0], lhs_buffer);
  type::Type lhs_type = context().qual_types(node->exprs()[0])[0].type();

  std::vector<ir::BasicBlock const *> phi_blocks;
  std::vector<ir::RegOr<bool>> phi_values;

  auto *land_block = current().subroutine->AppendBlock();
  for (size_t i = 0; i + 1 < node->ops().size(); ++i) {
    auto const *rhs_expr = node->exprs()[i + 1];
    rhs_buffer.clear();
    EmitToBuffer(rhs_expr, rhs_buffer);
    type::Type rhs_type = context().qual_types(rhs_expr)[0].type();

    auto cmp =
        EmitPair(*this, node, node->ops()[i], type::Typed(lhs_buffer, lhs_type),
                 type::Typed(rhs_buffer, rhs_type));

    phi_blocks.push_back(current_block());
    phi_values.push_back(false);
    auto *next_block = current().subroutine->AppendBlock();
    current_block()->set_jump(ir::JumpCmd::Cond(cmp, next_block, land_block));
    current_block() = next_block;
    lhs_buffer      = std::move(rhs_buffer);
    lhs_type        = rhs_type;
  }

  // Once more for the last element, but don't do a conditional jump.
  EmitToBuffer(node->exprs().back(), rhs_buffer);
  type::Type rhs_type = context().qual_types(node->exprs().back())[0].type();

  phi_blocks.push_back(current_block());
  phi_values.push_back(EmitPair(*this, node, node->ops().back(),
                                type::Typed(lhs_buffer, lhs_type),
                                type::Typed(rhs_buffer, rhs_type)));

  current_block()->set_jump(ir::JumpCmd::Uncond(land_block));

  current_block() = land_block;

  if (phi_values.size() == 1) {
    out.append(phi_values.back());
  } else {
    ir::PhiInstruction<bool> phi(std::move(phi_blocks), std::move(phi_values));
    phi.result = current().subroutine->Reserve();
    out.append(current_block()->Append(std::move(phi)));
  }
}

void Compiler::EmitCopyAssign(
    ast::ComparisonOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], type::Bool));
}

void Compiler::EmitMoveAssign(
    ast::ComparisonOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], type::Bool));
}

void Compiler::EmitCopyInit(
    ast::ComparisonOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyInitializationEmitter emitter(*this);
  emitter(to[0], buffer);
}

void Compiler::EmitMoveInit(
    ast::ComparisonOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveInitializationEmitter emitter(*this);
  emitter(to[0], buffer);
}

}  // namespace compiler
