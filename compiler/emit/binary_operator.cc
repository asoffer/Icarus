#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "diagnostic/message.h"
#include "type/cast.h"

namespace compiler {
namespace {

struct PatternMatchingFailed {
  static constexpr std::string_view kCategory = "pattern-match-error";
  static constexpr std::string_view kName     = "pattern-matching-failed";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Pattern matching failed"),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
};

template <template <typename> typename Op, typename... Ts>
void Apply(type::Typed<ast::Expression const *> lhs,
           type::Typed<ast::Expression const *> rhs, Compiler &c,
           ir::PartialResultBuffer &out) {
  auto meet       = type::Meet(lhs.type(), rhs.type());
  auto lhs_result = EmitCast(c, lhs, meet);
  auto rhs_result = EmitCast(c, rhs, meet);
  ApplyTypes<Ts...>(meet, [&]<typename T>() {
    out.append(c.current_block()->Append(
        Op<T>{.lhs    = lhs_result.back().get<T>(),
              .rhs    = rhs_result.back().get<T>(),
              .result = c.current().group->Reserve()}));
  });
}

void EmitBinaryOverload(Compiler &c, ast::BinaryOperator const *node,
                        ir::PartialResultBuffer &out) {
  // TODO: We claim ownership but later release the ownership. This is
  // safe and correct, but it's also a bit of a lie. It would be better
  // if we had a mechanism to hide ownership.
  std::array<ast::Call::Argument, 2> arguments{
      ast::Call::Argument("", std::unique_ptr<ast::Expression>(
                                  &const_cast<ast::Expression &>(node->lhs()))),
      ast::Call::Argument("",
                          std::unique_ptr<ast::Expression>(
                              &const_cast<ast::Expression &>(node->rhs())))};

  type::Type result_type = c.context().qual_types(node)[0].type();
  type::Typed<ir::RegOr<ir::addr_t>> result(c.state().TmpAlloca(result_type),
                                            result_type);

  EmitCall(c, c.context().CallMetadata(node).resolved(), {}, arguments,
           absl::MakeConstSpan(&result, 1));

  for (auto &argument : arguments) {
    auto &&[name, expr] = std::move(argument).extract();
    expr.release();
  }
  out.append(PtrFix(c.current(), result->reg(), result_type));
}

}  // namespace

void Compiler::EmitToBuffer(ast::BinaryOperator const *node,
                            ir::PartialResultBuffer &out) {
  switch (node->kind()) {
    case ast::BinaryOperator::Kind::Or: {
      auto lhs_ir      = EmitAs<bool>(&node->lhs());
      auto *land_block = current().group->AppendBlock();

      std::vector<ir::BasicBlock const *> phi_blocks;

      auto *next_block = current().group->AppendBlock();
      current_block()->set_jump(
          ir::JumpCmd::Cond(lhs_ir, land_block, next_block));
      phi_blocks.push_back(current_block());
      current_block() = next_block;

      auto rhs_ir = EmitAs<bool>(&node->rhs());
      phi_blocks.push_back(current_block());
      current_block()->set_jump(ir::JumpCmd::Uncond(land_block));

      current_block() = land_block;

      ir::PhiInstruction<bool> phi(std::move(phi_blocks), {true, rhs_ir});
      phi.result = current().group->Reserve();
      out.append(current_block()->Append(std::move(phi)));
      return;
    } break;
    case ast::BinaryOperator::Kind::SymbolOr: {
      type::Type lhs_type = context().qual_types(&node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(&node->rhs())[0].type();
      if (lhs_type.is<type::Flags>() and lhs_type == rhs_type) {
        auto lhs_ir = EmitAs<type::Flags::underlying_type>(&node->lhs());
        auto rhs_ir = EmitAs<type::Flags::underlying_type>(&node->rhs());
        out.append(current_block()->Append(type::OrFlagsInstruction{
            .lhs    = lhs_ir,
            .rhs    = rhs_ir,
            .result = current().group->Reserve()}));
      } else {
        EmitBinaryOverload(*this, node, out);
      }
      return;
    } break;
    case ast::BinaryOperator::Kind::Xor: {
      auto lhs_ir = EmitAs<bool>(&node->lhs());
      auto rhs_ir = EmitAs<bool>(&node->rhs());
      out.append(current_block()->Append(ir::NeInstruction<bool>{
          .lhs    = lhs_ir,
          .rhs    = rhs_ir,
          .result = current().group->Reserve()}));
      return;
    } break;
    case ast::BinaryOperator::Kind::SymbolXor: {
      type::Type lhs_type = context().qual_types(&node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(&node->rhs())[0].type();
      if (lhs_type.is<type::Flags>() and lhs_type == rhs_type) {
        auto lhs_ir = EmitAs<type::Flags::underlying_type>(&node->lhs());
        auto rhs_ir = EmitAs<type::Flags::underlying_type>(&node->rhs());
        out.append(current_block()->Append(type::XorFlagsInstruction{
            .lhs    = lhs_ir,
            .rhs    = rhs_ir,
            .result = current().group->Reserve()}));
      } else {
        EmitBinaryOverload(*this, node, out);
      }
      return;
    } break;
    case ast::BinaryOperator::Kind::And: {
      auto lhs_ir = EmitAs<bool>(&node->lhs());
      auto rhs_ir = EmitAs<bool>(&node->rhs());

      auto *land_block = current().group->AppendBlock();

      std::vector<ir::BasicBlock const *> phi_blocks;

      auto *next_block = current().group->AppendBlock();
      current_block()->set_jump(
          ir::JumpCmd::Cond(lhs_ir, next_block, land_block));
      phi_blocks.push_back(current_block());
      current_block() = next_block;

      phi_blocks.push_back(current_block());
      current_block()->set_jump(ir::JumpCmd::Uncond(land_block));

      current_block() = land_block;
      ir::PhiInstruction<bool> phi(std::move(phi_blocks), {false, rhs_ir});
      phi.result = current().group->Reserve();
      out.append(current_block()->Append(std::move(phi)));
      return;
    } break;
    case ast::BinaryOperator::Kind::SymbolAnd: {
      type::Type lhs_type = context().qual_types(&node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(&node->rhs())[0].type();
      if (lhs_type.is<type::Flags>() and lhs_type == rhs_type) {
        auto lhs_ir = EmitAs<type::Flags::underlying_type>(&node->lhs());
        auto rhs_ir = EmitAs<type::Flags::underlying_type>(&node->rhs());
        out.append(current_block()->Append(type::AndFlagsInstruction{
            .lhs    = lhs_ir,
            .rhs    = rhs_ir,
            .result = current().group->Reserve()}));
        return;
      } else {
        EmitBinaryOverload(*this, node, out);
      }
    } break;
    case ast::BinaryOperator::Kind::Add: {
      auto typed_lhs = context().typed(&node->lhs());
      auto typed_rhs = context().typed(&node->rhs());
      if (auto const *lhs_buf_ptr_type =
              typed_lhs.type().if_as<type::BufferPointer>();
          lhs_buf_ptr_type and type::IsIntegral(typed_rhs.type())) {
        auto lhs_ir = EmitAs<ir::addr_t>(&node->lhs(), out);
        // TODO: Remove assumption that the pointer difference type is int64_t.
        auto rhs_ir =
            EmitCast(*this, typed_rhs,
                     type::PointerDifferenceType(resources().architecture))
                .back()
                .get<int64_t>();
        out.append(current_block()->Append(ir::PtrIncrInstruction{
            .addr   = lhs_ir,
            .index  = rhs_ir,
            .ptr    = lhs_buf_ptr_type,
            .result = current().group->Reserve()}));
      } else if (auto const *rhs_buf_ptr_type =
                     typed_rhs.type().if_as<type::BufferPointer>();
                 rhs_buf_ptr_type and type::IsIntegral(typed_lhs.type())) {
        // TODO: Remove assumption that the pointer difference type is int64_t.
        auto lhs_ir =
            EmitCast(*this, typed_lhs,
                     type::PointerDifferenceType(resources().architecture))
                .back()
                .get<int64_t>();
        auto rhs_ir = EmitAs<ir::addr_t>(&node->rhs(), out);
        out.append(current_block()->Append(ir::PtrIncrInstruction{
            .addr   = rhs_ir,
            .index  = lhs_ir,
            .ptr    = rhs_buf_ptr_type,
            .result = current().group->Reserve()}));
      } else if (typed_lhs.type().is<type::Primitive>() and
                 typed_rhs.type().is<type::Primitive>()) {
        Apply<ir::AddInstruction, ir::Integer, int8_t, int16_t, int32_t,
              int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float, double>(
            typed_lhs, typed_rhs, *this, out);
      } else {
        EmitBinaryOverload(*this, node, out);
      }
      return;
    } break;
    case ast::BinaryOperator::Kind::Sub: {
      auto typed_lhs = context().typed(&node->lhs());
      auto typed_rhs = context().typed(&node->rhs());
      if (auto const *lhs_buf_ptr_type = typed_lhs.type().if_as<type::BufferPointer>();
          lhs_buf_ptr_type and type::IsIntegral(typed_rhs.type())) {
        auto lhs_ir = EmitAs<ir::addr_t>(&node->lhs(), out);
        // TODO: Remove assumption that the pointer difference type is int64_t.
        auto rhs_ir =
            EmitCast(*this, typed_rhs,
                     type::PointerDifferenceType(resources().architecture))
                .back()
                .get<int64_t>();
        out.append(current_block()->Append(ir::PtrIncrInstruction{
            .addr   = lhs_ir,
            .index  = current_block()->Append(ir::NegInstruction<int64_t>{
                .operand = rhs_ir,
                .result  = current().group->Reserve(),
            }),
            .ptr    = lhs_buf_ptr_type,
            .result = current().group->Reserve()}));
      } else if (auto const *rhs_buf_ptr_type =
                     typed_rhs.type().if_as<type::BufferPointer>();
                 rhs_buf_ptr_type and type::IsIntegral(typed_lhs.type())) {
        // TODO: Remove assumption that the pointer difference type is int64_t.
        auto lhs_ir =
            EmitCast(*this, typed_lhs,
                     type::PointerDifferenceType(resources().architecture))
                .back()
                .get<int64_t>();
        auto rhs_ir = EmitAs<ir::addr_t>(&node->rhs(), out);
        out.append(current_block()->Append(ir::PtrIncrInstruction{
            .addr   = rhs_ir,
            .index  = current_block()->Append(ir::NegInstruction<int64_t>{
                .operand = lhs_ir,
                .result  = current().group->Reserve(),
            }),
            .ptr    = rhs_buf_ptr_type,
            .result = current().group->Reserve()}));
      } else if (auto const *buf_ptr = typed_lhs.type().if_as<type::BufferPointer>();
                 typed_lhs.type() == typed_rhs.type() and buf_ptr) {
        auto lhs_ir = EmitAs<ir::addr_t>(&node->lhs());
        auto rhs_ir = EmitAs<ir::addr_t>(&node->rhs());
        out.append(current_block()->Append(ir::PtrDiffInstruction{
            .lhs          = lhs_ir,
            .rhs          = rhs_ir,
            .pointee_type = buf_ptr->pointee(),
            .result       = current().group->Reserve()}));
      } else if (typed_lhs.type().is<type::Primitive>() and
                 typed_rhs.type().is<type::Primitive>()) {
        Apply<ir::SubInstruction, ir::Integer, int8_t, int16_t, int32_t,
              int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float, double>(
            typed_lhs, typed_rhs, *this, out);
      } else {
        EmitBinaryOverload(*this, node, out);
      }
    } break;
    case ast::BinaryOperator::Kind::Mul: {
      type::Type lhs_type = context().qual_types(&node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(&node->rhs())[0].type();
      if (lhs_type.is<type::Primitive>() and rhs_type.is<type::Primitive>()) {
        Apply<ir::MulInstruction, ir::Integer, int8_t, int16_t, int32_t,
              int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float, double>(
            type::Typed(&node->lhs(), lhs_type),
            type::Typed(&node->rhs(), rhs_type), *this, out);
      } else {
        EmitBinaryOverload(*this, node, out);
      }
    } break;
    case ast::BinaryOperator::Kind::Div: {
      type::Type lhs_type = context().qual_types(&node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(&node->rhs())[0].type();
      if (lhs_type.is<type::Primitive>() and rhs_type.is<type::Primitive>()) {
        Apply<ir::DivInstruction, ir::Integer, int8_t, int16_t, int32_t,
              int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float, double>(
            type::Typed(&node->lhs(), lhs_type),
            type::Typed(&node->rhs(), rhs_type), *this, out);
      } else {
        EmitBinaryOverload(*this, node, out);
      }
    } break;
    case ast::BinaryOperator::Kind::Mod: {
      type::Type lhs_type = context().qual_types(&node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(&node->rhs())[0].type();
      if (lhs_type.is<type::Primitive>() and rhs_type.is<type::Primitive>()) {
        Apply<ir::ModInstruction, ir::Integer, int8_t, int16_t, int32_t,
              int64_t, uint8_t, uint16_t, uint32_t, uint64_t>(
            type::Typed(&node->lhs(), lhs_type),
            type::Typed(&node->rhs(), rhs_type), *this, out);
      } else {
        EmitBinaryOverload(*this, node, out);
      }
    } break;
    case ast::BinaryOperator::Kind::BlockJump: {
      ir::PartialResultBuffer buffer;
      auto block = *EvaluateOrDiagnoseAs<ir::Block>(&node->rhs());
      ASSERT(state().scopes.size() != 0u);
      ir::Scope scope    = state().scopes.back();
      ir::PartialResultBuffer lhs_buffer;
      EmitToBuffer(&node->lhs(), lhs_buffer);
      type::Type lhs_type = context().qual_types(&node->lhs())[0].type();
      scope.add_parameters(
          block, RegisterReferencing(current(), lhs_type, lhs_buffer[0]));

      auto *exit = current().group->AppendBlock();
      current_block()->set_jump(ir::JumpCmd::ToBlock(block, exit));
      current_block() = exit;
    }
  }
}

void Compiler::EmitToBuffer(ast::BinaryAssignmentOperator const *node,
                            ir::PartialResultBuffer &) {
  auto lhs_lval = EmitRef(&node->lhs());

  switch (node->kind()) {
    case ast::BinaryOperator::Kind::SymbolOr:
      current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
          .value    = current_block()->Append(type::OrFlagsInstruction{
              .lhs    = current_block()->Append(ir::LoadInstruction{
                  .type   = type::Flags::UnderlyingType(),
                  .addr   = lhs_lval,
                  .result = current().group->Reserve()}),
              .rhs    = EmitAs<type::Flags::underlying_type>(&node->rhs()),
              .result = current().group->Reserve()}),
          .location = lhs_lval});
      return;
    case ast::BinaryOperator::Kind::SymbolAnd:
      current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
          .value    = current_block()->Append(type::AndFlagsInstruction{
              .lhs    = current_block()->Append(ir::LoadInstruction{
                  .type   = type::Flags::UnderlyingType(),
                  .addr   = lhs_lval,
                  .result = current().group->Reserve()}),
              .rhs    = EmitAs<type::Flags::underlying_type>(&node->rhs()),
              .result = current().group->Reserve()}),
          .location = lhs_lval});
      return;
    case ast::BinaryOperator::Kind::SymbolXor:
      current_block()->Append(ir::StoreInstruction<type::Flags::underlying_type>{
          .value    = current_block()->Append(type::XorFlagsInstruction{
              .lhs    = current_block()->Append(ir::LoadInstruction{
                  .type   = type::Flags::UnderlyingType(),
                  .addr   = lhs_lval,
                  .result = current().group->Reserve()}),
              .rhs    = EmitAs<type::Flags::underlying_type>(&node->rhs()),
              .result = current().group->Reserve()}),
          .location = lhs_lval});
      return;
    case ast::BinaryOperator::Kind::Add: {
      ir::PartialResultBuffer buffer;
      EmitToBuffer(&node->rhs(), buffer);
      type::Type lhs_type = context().qual_types(&node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(&node->rhs())[0].type();
      if (auto const *lhs_buf_ptr_type = lhs_type.if_as<type::BufferPointer>();
          lhs_buf_ptr_type and type::IsIntegral(rhs_type)) {
        // TODO: Remove assumption that the pointer difference type is int64_t.
        EmitCast(current(), rhs_type,
                 type::PointerDifferenceType(resources().architecture), buffer);
        current_block()->Append(ir::StoreInstruction<ir::addr_t>{
            .value    = current_block()->Append(ir::PtrIncrInstruction{
                .addr   = current_block()->Append(ir::LoadInstruction{
                    .type   = lhs_buf_ptr_type->pointee(),
                    .addr   = lhs_lval,
                    .result = current().group->Reserve(),
                }),
                .index  = buffer.back().get<int64_t>(),
                .ptr    = lhs_buf_ptr_type,
                .result = current().group->Reserve()}),
            .location = lhs_lval});
      } else {
        EmitCast(current(), rhs_type, lhs_type, buffer);
        ir::Reg loaded = current_block()->Append(ir::LoadInstruction{
            .type   = lhs_type,
            .addr   = lhs_lval,
            .result = current().group->Reserve(),
        });
        ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                   uint16_t, uint32_t, uint64_t, float, double>(
            lhs_type, [&]<typename T>() {
              current_block()->Append(ir::StoreInstruction<T>{
                  .value    = current_block()->Append(ir::AddInstruction<T>{
                      .lhs    = loaded,
                      .rhs    = buffer.back().get<T>(),
                      .result = current().group->Reserve()}),
                  .location = lhs_lval});
            });
      }
      return;
    } break;
    case ast::BinaryOperator::Kind::Sub: {
      ir::PartialResultBuffer buffer;
      EmitToBuffer(&node->rhs(), buffer);
      type::Type lhs_type = context().qual_types(&node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(&node->rhs())[0].type();
      if (auto const *lhs_buf_ptr_type = lhs_type.if_as<type::BufferPointer>();
          lhs_buf_ptr_type and type::IsIntegral(rhs_type)) {
        // TODO: Remove assumption that the pointer difference type is int64_t.
        current_block()->Append(ir::StoreInstruction<ir::addr_t>{
            .value    = current_block()->Append(ir::PtrIncrInstruction{
                .addr   = current_block()->Append(ir::LoadInstruction{
                    .type   = lhs_buf_ptr_type->pointee(),
                    .addr   = lhs_lval,
                    .result = current().group->Reserve(),
                }),
                .index  = current_block()->Append(ir::NegInstruction<int64_t>{
                    .operand = buffer.back().get<int64_t>(),
                    .result  = current().group->Reserve(),
                }),
                .ptr    = lhs_buf_ptr_type,
                .result = current().group->Reserve()}),
            .location = lhs_lval});
      } else {
        EmitCast(current(), rhs_type, lhs_type, buffer);
        ir::Reg loaded = current_block()->Append(ir::LoadInstruction{
            .type   = lhs_type,
            .addr   = lhs_lval,
            .result = current().group->Reserve(),
        });
        ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                   uint16_t, uint32_t, uint64_t, float, double>(
            lhs_type, [&]<typename T>() {
              current_block()->Append(ir::StoreInstruction<T>{
                  .value    = current_block()->Append(ir::SubInstruction<T>{
                      .lhs    = loaded,
                      .rhs    = buffer.back().get<T>(),
                      .result = current().group->Reserve()}),
                  .location = lhs_lval});
            });
      }
      return;
    } break;
    case ast::BinaryOperator::Kind::Mul: {
      type::Type lhs_type = context().qual_types(&node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(&node->rhs())[0].type();

      ir::PartialResultBuffer buffer;
      EmitToBuffer(&node->rhs(), buffer);
      EmitCast(current(), rhs_type, lhs_type, buffer);
      ir::Reg loaded = current_block()->Append(ir::LoadInstruction{
          .type   = lhs_type,
          .addr   = lhs_lval,
          .result = current().group->Reserve(),
      });
      ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t,
                 uint64_t, float, double>(lhs_type, [&]<typename T>() {
        current_block()->Append(ir::StoreInstruction<T>{
            .value    = current_block()->Append(ir::MulInstruction<T>{
                .lhs    = loaded,
                .rhs    = buffer.back().get<T>(),
                .result = current().group->Reserve()}),
            .location = lhs_lval});
      });
      return;
    } break;
    case ast::BinaryOperator::Kind::Div: {
      type::Type lhs_type = context().qual_types(&node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(&node->rhs())[0].type();

      ir::PartialResultBuffer buffer;
      EmitToBuffer(&node->rhs(), buffer);
      EmitCast(current(), rhs_type, lhs_type, buffer);
      ir::Reg loaded = current_block()->Append(ir::LoadInstruction{
          .type   = lhs_type,
          .addr   = lhs_lval,
          .result = current().group->Reserve(),
      });
      ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                 uint16_t, uint32_t, uint64_t, float, double>(
          lhs_type, [&]<typename T>() {
            current_block()->Append(ir::StoreInstruction<T>{
                .value    = current_block()->Append(ir::DivInstruction<T>{
                    .lhs    = loaded,
                    .rhs    = buffer.back().get<T>(),
                    .result = current().group->Reserve()}),
                .location = lhs_lval});
          });
      return;
    } break;
    case ast::BinaryOperator::Kind::Mod: {
      type::Type lhs_type = context().qual_types(&node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(&node->rhs())[0].type();

      ir::PartialResultBuffer buffer;
      EmitToBuffer(&node->rhs(), buffer);
      EmitCast(current(), rhs_type, lhs_type, buffer);
      ir::Reg loaded = current_block()->Append(ir::LoadInstruction{
          .type   = lhs_type,
          .addr   = lhs_lval,
          .result = current().group->Reserve(),
      });
      ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                 uint16_t, uint32_t, uint64_t>(lhs_type, [&]<typename T>() {
        current_block()->Append(ir::StoreInstruction<T>{
            .value    = current_block()->Append(ir::ModInstruction<T>{
                .lhs    = loaded,
                .rhs    = buffer.back().get<T>(),
                .result = current().group->Reserve()}),
            .location = lhs_lval});
      });
      return;
    } break;
    default: {
      UNREACHABLE(node->DebugString());
    } break;
  }
}

void Compiler::EmitCopyInit(
    ast::BinaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyAssign(type::Typed<ir::RegOr<ir::addr_t>>(*to[0], t),
                 type::Typed(buffer[0], t));
}

void Compiler::EmitMoveInit(
    ast::BinaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveAssign(type::Typed<ir::RegOr<ir::addr_t>>(*to[0], t),
                 type::Typed(buffer[0], t));
}

void Compiler::EmitMoveAssign(
    ast::BinaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveAssign(type::Typed<ir::RegOr<ir::addr_t>>(*to[0], t),
                 type::Typed(buffer[0], t));
}

void Compiler::EmitCopyAssign(
    ast::BinaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyAssign(type::Typed<ir::RegOr<ir::addr_t>>(*to[0], t),
                 type::Typed(buffer[0], t));
}

bool Compiler::PatternMatch(
    ast::BinaryOperator const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::CompleteResultBuffer>
        &bindings) {
  std::optional<std::variant<ir::CompleteResultBuffer,
                             std::vector<diagnostic::ConsumedMessage>>>
      lhs_buffer, rhs_buffer;
  if (not node->lhs().covers_binding()) {
    lhs_buffer = EvaluateToBuffer(
        type::Typed<ast::Expression const *>(&node->lhs(), pmc.type));
  }

  if (not node->rhs().covers_binding()) {
    rhs_buffer = EvaluateToBuffer(
        type::Typed<ast::Expression const *>(&node->rhs(), pmc.type));
  }

  switch (node->kind()) {
    case ast::BinaryOperator::Kind::Add: {
      auto const *p = pmc.type.if_as<type::Primitive>();
      if (p and lhs_buffer) {
        p->Apply([&]<typename T>() {
          auto sum  = pmc.value.template get<T>(0);
          auto term = std::get<ir::CompleteResultBuffer>(*lhs_buffer)
                          .template get<T>(0);
          if constexpr (std::is_arithmetic_v<T>) {
            pmc.value.clear();
            pmc.value.append(static_cast<T>(sum - term));
          }
        });
        return PatternMatch(&node->rhs(), pmc, bindings);
      } else if (p and rhs_buffer) {
        p->Apply([&]<typename T>() {
          auto sum  = pmc.value.template get<T>(0);
          auto term = std::get<ir::CompleteResultBuffer>(*rhs_buffer)
                          .template get<T>(0);
          if constexpr (std::is_arithmetic_v<T>) {
            pmc.value.clear();
            pmc.value.append(static_cast<T>(sum - term));
          }
        });
        return PatternMatch(&node->lhs(), pmc, bindings);
      } else {
        // TODO: Explain that we cannot match because the pattern is not
        // sufficiently simple.
        diag().Consume(PatternMatchingFailed{.range = node->range()});
        return false;
      }
    } break;
    case ast::BinaryOperator::Kind::Sub: {
      auto const *p = pmc.type.if_as<type::Primitive>();
      if (p and lhs_buffer) {
        p->Apply([&]<typename T>() {
          auto diff = pmc.value.template get<T>(0);
          auto term = std::get<ir::CompleteResultBuffer>(*lhs_buffer)
                          .template get<T>(0);
          if constexpr (std::is_integral_v<T>) {
            pmc.value.clear();
            pmc.value.append(static_cast<T>(term - diff));
          }
        });
        return PatternMatch(&node->rhs(), pmc, bindings);
      } else if (p and rhs_buffer) {
        p->Apply([&]<typename T>() {
          auto diff = pmc.value.template get<T>(0);
          auto term = std::get<ir::CompleteResultBuffer>(*rhs_buffer)
                          .template get<T>(0);
          if constexpr (std::is_integral_v<T>) {
            pmc.value.clear();
            pmc.value.append(static_cast<T>(diff + term));
          }
        });
        return PatternMatch(&node->lhs(), pmc, bindings);
      } else {
        // TODO: Explain that we cannot match because the pattern is not
        // sufficiently simple.
        diag().Consume(PatternMatchingFailed{.range = node->range()});
      }
    } break;
    case ast::BinaryOperator::Kind::Mul: {
      ast::Expression const *expr;
      auto const *p = pmc.type.if_as<type::Primitive>();
      if (p and lhs_buffer) {
        expr = &node->rhs();
        p->Apply([&]<typename T>() {
          auto product = pmc.value.template get<T>(0);
          auto factor  = std::get<ir::CompleteResultBuffer>(*lhs_buffer)
                            .template get<T>(0);
          if constexpr (std::is_integral_v<T>) {
            if (product % factor == 0) { expr = nullptr; }
          }
          if constexpr (std::is_arithmetic_v<T>) {
            pmc.value.clear();
            pmc.value.append(static_cast<T>(product / factor));
          }
        });
        return PatternMatch(&node->rhs(), pmc, bindings);
      } else if (p and rhs_buffer) {
        expr = &node->lhs();
        p->Apply([&]<typename T>() {
          auto product = pmc.value.template get<T>(0);
          auto factor  = std::get<ir::CompleteResultBuffer>(*rhs_buffer)
                            .template get<T>(0);
          if constexpr (std::is_integral_v<T>) {
            if (product % factor == 0) { expr = nullptr; }
          }
          if constexpr (std::is_arithmetic_v<T>) {
            pmc.value.clear();
            pmc.value.append(static_cast<T>(product / factor));
          }
        });

        if (expr) {
          return PatternMatch(expr, pmc, bindings);
        } else {
          diag().Consume(PatternMatchingFailed{.range = node->range()});
          return false;
        }
      } else {
        NOT_YET();
      }
    } break;
    default: NOT_YET();
  }
  UNREACHABLE();
}

}  // namespace compiler
