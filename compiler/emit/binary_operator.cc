#include "ast/ast.h"
#include "compiler/compiler.h"
#include "diagnostic/message.h"

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
  ApplyTypes<Ts...>(type::Meet(lhs.type(), rhs.type()), [&]<typename T>() {
    ir::RegOr<T> l = c.EmitWithCastTo<T>(lhs.type(), *lhs);
    ir::RegOr<T> r = c.EmitWithCastTo<T>(rhs.type(), *rhs);
    out.append(c.current_block()->Append(Op<T>{
        .lhs = l, .rhs = r, .result = c.builder().CurrentGroup()->Reserve()}));
  });
}

}  // namespace

void Compiler::EmitToBuffer(ast::BinaryOperator const *node,
                            ir::PartialResultBuffer &out) {
  switch (node->op()) {
    case frontend::Operator::Or: {
      auto lhs_ir      = EmitAs<bool>(node->lhs());
      auto *land_block = builder().AddBlock();

      std::vector<ir::BasicBlock const *> phi_blocks;

      auto *next_block = builder().AddBlock();
      builder().CondJump(lhs_ir, land_block, next_block);
      phi_blocks.push_back(builder().CurrentBlock());
      builder().CurrentBlock() = next_block;

      auto rhs_ir = EmitAs<bool>(node->rhs());
      phi_blocks.push_back(builder().CurrentBlock());
      builder().UncondJump(land_block);

      builder().CurrentBlock() = land_block;

      out.append(builder().Phi<bool>(std::move(phi_blocks), {true, rhs_ir}));
      return;
    } break;
    case frontend::Operator::SymbolOr: {
      auto lhs_ir = EmitAs<type::Flags::underlying_type>(node->lhs());
      auto rhs_ir = EmitAs<type::Flags::underlying_type>(node->rhs());
      // `|` is not overloadable, and blocks piped together must be done
      // syntactically in a `goto` node and are handled by the parser.
      out.append(current_block()->Append(type::OrFlagsInstruction{
          .lhs    = lhs_ir,
          .rhs    = rhs_ir,
          .result = builder().CurrentGroup()->Reserve()}));
      return;
    } break;
    case frontend::Operator::Xor: {
      auto lhs_ir = EmitAs<bool>(node->lhs());
      auto rhs_ir = EmitAs<bool>(node->rhs());
      out.append(builder().Ne(lhs_ir, rhs_ir));
      return;
    } break;
    case frontend::Operator::SymbolXor: {
      auto lhs_ir = EmitAs<type::Flags::underlying_type>(node->lhs());
      auto rhs_ir = EmitAs<type::Flags::underlying_type>(node->rhs());
      out.append(current_block()->Append(type::XorFlagsInstruction{
          .lhs    = lhs_ir,
          .rhs    = rhs_ir,
          .result = builder().CurrentGroup()->Reserve()}));
      return;
    } break;
    case frontend::Operator::And: {
      auto lhs_ir = EmitAs<bool>(node->lhs());
      auto rhs_ir = EmitAs<bool>(node->rhs());

      auto *land_block = builder().AddBlock();

      std::vector<ir::BasicBlock const *> phi_blocks;

      auto *next_block = builder().AddBlock();
      builder().CondJump(lhs_ir, next_block, land_block);
      phi_blocks.push_back(builder().CurrentBlock());
      builder().CurrentBlock() = next_block;

      phi_blocks.push_back(builder().CurrentBlock());
      builder().UncondJump(land_block);

      builder().CurrentBlock() = land_block;

      out.append(builder().Phi<bool>(std::move(phi_blocks), {false, rhs_ir}));
      return;
    } break;
    case frontend::Operator::SymbolAnd: {
      auto t      = context().qual_types(node)[0].type();
      if (t == type::Bool) {
      auto lhs_ir = EmitAs<bool>(node->lhs());
      auto rhs_ir = EmitAs<bool>(node->rhs());

        auto *land_block = builder().AddBlock();

        std::vector<ir::BasicBlock const *> phi_blocks;

        auto *next_block = builder().AddBlock();
        builder().CondJump(lhs_ir, next_block, land_block);
        phi_blocks.push_back(builder().CurrentBlock());
        builder().CurrentBlock() = next_block;

        phi_blocks.push_back(builder().CurrentBlock());
        builder().UncondJump(land_block);

        builder().CurrentBlock() = land_block;

        out.append(builder().Phi<bool>(std::move(phi_blocks), {false, rhs_ir}));
        return;
      } else if (t.is<type::Flags>()) {
        auto lhs_ir = EmitAs<type::Flags::underlying_type>(node->lhs());
        auto rhs_ir = EmitAs<type::Flags::underlying_type>(node->rhs());

        // `|` is not overloadable, and blocks piped together must be done
        // syntactically in a `goto` node and are handled by the parser.
        out.append(current_block()->Append(type::AndFlagsInstruction{
            .lhs    = lhs_ir,
            .rhs    = rhs_ir,
            .result = builder().CurrentGroup()->Reserve()}));
        return;
      } else {
        UNREACHABLE();
      }
    } break;
    case frontend::Operator::Add: {
      type::Type lhs_type = context().qual_types(node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(node->rhs())[0].type();
      if (auto const *lhs_buf_ptr_type = lhs_type.if_as<type::BufferPointer>();
          lhs_buf_ptr_type and type::IsIntegral(rhs_type)) {
        auto lhs_ir = EmitAs<ir::addr_t>(node->lhs(), out);
        auto rhs_ir = EmitWithCastTo<int64_t>(rhs_type, node->rhs(), out);
        out.append(builder().PtrIncr(lhs_ir, rhs_ir, lhs_buf_ptr_type));
      } else if (auto const *rhs_buf_ptr_type =
                     rhs_type.if_as<type::BufferPointer>();
                 rhs_buf_ptr_type and type::IsIntegral(lhs_type)) {
        auto lhs_ir = EmitWithCastTo<int64_t>(lhs_type, node->lhs(), out);
        auto rhs_ir = EmitAs<ir::addr_t>(node->rhs(), out);
        out.append(builder().PtrIncr(rhs_ir, lhs_ir, rhs_buf_ptr_type));
      } else {
        Apply<ir::AddInstruction, ir::Integer, int8_t, int16_t, int32_t,
              int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float, double>(
            type::Typed(node->lhs(), lhs_type),
            type::Typed(node->rhs(), rhs_type), *this, out);
      }
      return;
    } break;
    case frontend::Operator::Sub: {
      type::Type lhs_type = context().qual_types(node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(node->rhs())[0].type();
      if (auto const *lhs_buf_ptr_type = lhs_type.if_as<type::BufferPointer>();
         lhs_buf_ptr_type and type::IsIntegral(rhs_type)) {
        auto lhs_ir = EmitAs<ir::addr_t>(node->lhs(), out);
        auto rhs_ir = EmitWithCastTo<int64_t>(rhs_type, node->rhs(), out);
        out.append(
            builder().PtrIncr(lhs_ir, builder().Neg(rhs_ir), lhs_buf_ptr_type));
      } else if (auto const *rhs_buf_ptr_type =
                     rhs_type.if_as<type::BufferPointer>();
                 rhs_buf_ptr_type and type::IsIntegral(lhs_type)) {
        auto lhs_ir = EmitWithCastTo<int64_t>(lhs_type, node->lhs(), out);
        auto rhs_ir = EmitAs<ir::addr_t>(node->rhs(), out);
        out.append(
            builder().PtrIncr(rhs_ir, builder().Neg(lhs_ir), rhs_buf_ptr_type));
      } else if (auto const *buf_ptr = lhs_type.if_as<type::BufferPointer>();
                 lhs_type == rhs_type and buf_ptr) {
        auto lhs_ir = EmitAs<ir::addr_t>(node->lhs());
        auto rhs_ir = EmitAs<ir::addr_t>(node->rhs());
        out.append(current_block()->Append(ir::PtrDiffInstruction{
            .lhs          = lhs_ir,
            .rhs          = rhs_ir,
            .pointee_type = buf_ptr->pointee(),
            .result       = builder().CurrentGroup()->Reserve()}));
      } else {
        Apply<ir::SubInstruction, ir::Integer, int8_t, int16_t, int32_t,
              int64_t, uint8_t, uint16_t, uint32_t, uint64_t, float, double>(
            type::Typed(node->lhs(), lhs_type),
            type::Typed(node->rhs(), rhs_type), *this, out);
      }
    } break;
    case frontend::Operator::Mul: {
      type::Type lhs_type = context().qual_types(node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(node->rhs())[0].type();
      Apply<ir::MulInstruction, ir::Integer, int8_t, int16_t, int32_t, int64_t,
            uint8_t, uint16_t, uint32_t, uint64_t, float, double>(
          type::Typed(node->lhs(), lhs_type), type::Typed(node->rhs(), rhs_type), *this,
          out);
    } break;
    case frontend::Operator::Div: {
      type::Type lhs_type = context().qual_types(node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(node->rhs())[0].type();
      Apply<ir::DivInstruction, ir::Integer, int8_t, int16_t, int32_t, int64_t,
            uint8_t, uint16_t, uint32_t, uint64_t, float, double>(
          type::Typed(node->lhs(), lhs_type),
          type::Typed(node->rhs(), rhs_type), *this, out);
    } break;
    case frontend::Operator::Mod: {
      type::Type lhs_type = context().qual_types(node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(node->rhs())[0].type();
      Apply<ir::ModInstruction, ir::Integer, int8_t, int16_t, int32_t, int64_t,
            uint8_t, uint16_t, uint32_t, uint64_t>(
          type::Typed(node->lhs(), lhs_type),
          type::Typed(node->rhs(), rhs_type), *this, out);
    } break;
    case frontend::Operator::SymbolOrEq: {
      auto this_type = context().qual_types(node)[0].type();
      auto lhs_lval  = EmitRef(node->lhs());
      if (this_type == type::Bool) {
        auto *land_block = builder().AddBlock();
        auto *more_block = builder().AddBlock();

        auto lhs_val       = builder().Load<bool>(lhs_lval);
        auto lhs_end_block = builder().CurrentBlock();
        builder().CondJump(lhs_val, land_block, more_block);

        builder().CurrentBlock() = more_block;
        auto rhs_val             = EmitAs<bool>(node->rhs());
        auto rhs_end_block       = builder().CurrentBlock();
        builder().UncondJump(land_block);

        builder().CurrentBlock() = land_block;
        builder().Store(builder().Phi<bool>({lhs_end_block, rhs_end_block},
                                            {ir::RegOr<bool>(true), rhs_val}),
                        lhs_lval);
      } else if (this_type.is<type::Flags>()) {
        builder().Store<ir::RegOr<type::Flags::underlying_type>>(
            current_block()->Append(type::OrFlagsInstruction{
                .lhs = builder().Load<type::Flags::underlying_type>(lhs_lval),
                .rhs = EmitAs<type::Flags::underlying_type>(node->rhs()),
                .result = builder().CurrentGroup()->Reserve()}),
            lhs_lval);
      } else {
        UNREACHABLE(this_type.to_string());
      }
      return;
    } break;
    case frontend::Operator::SymbolAndEq: {
      auto this_type = context().qual_types(node)[0].type();
      auto lhs_lval  = EmitRef(node->lhs());
      if (this_type.is<type::Flags>()) {
        builder().Store<ir::RegOr<type::Flags::underlying_type>>(
            current_block()->Append(type::AndFlagsInstruction{
                .lhs = builder().Load<type::Flags::underlying_type>(lhs_lval),
                .rhs = EmitAs<type::Flags::underlying_type>(node->rhs()),
                .result = builder().CurrentGroup()->Reserve()}),
            lhs_lval);
      } else if (this_type == type::Bool) {
        auto *land_block = builder().AddBlock();
        auto *more_block = builder().AddBlock();

        auto lhs_val       = builder().Load<bool>(lhs_lval);
        auto lhs_end_block = builder().CurrentBlock();
        builder().CondJump(lhs_val, more_block, land_block);

        builder().CurrentBlock() = more_block;
        auto rhs_val             = EmitAs<bool>(node->rhs());
        auto rhs_end_block = builder().CurrentBlock();
        builder().UncondJump(land_block);

        builder().CurrentBlock() = land_block;
        builder().Store(builder().Phi<bool>({lhs_end_block, rhs_end_block},
                                            {ir::RegOr<bool>(false), rhs_val}),
                        lhs_lval);
      } else {
        UNREACHABLE(this_type.to_string());
      }
      return;
    } break;
    case frontend::Operator::SymbolXorEq: {
      auto this_type = context().qual_types(node)[0].type();
      auto lhs_lval  = EmitRef(node->lhs());
      if (this_type.is<type::Flags>()) {
        auto rhs_ir = EmitAs<type::Flags::underlying_type>(node->rhs());
        builder().Store<ir::RegOr<type::Flags::underlying_type>>(
            current_block()->Append(type::XorFlagsInstruction{
                .lhs = builder().Load<type::Flags::underlying_type>(lhs_lval),
                .rhs = rhs_ir,
                .result = builder().CurrentGroup()->Reserve()}),
            lhs_lval);
      } else if (this_type == type::Bool) {
        auto rhs_ir = EmitAs<bool>(node->rhs());
        builder().Store(builder().Ne(builder().Load<bool>(lhs_lval), rhs_ir),
                        lhs_lval);
      } else {
        UNREACHABLE(this_type.to_string());
      }
      return;
    } break;
    case frontend::Operator::AddEq: {
      auto lhs_lval       = EmitRef(node->lhs());
      EmitToBuffer(node->rhs(), out);
      type::Type lhs_type = context().qual_types(node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(node->rhs())[0].type();
      if (auto const *lhs_buf_ptr_type = lhs_type.if_as<type::BufferPointer>();
          lhs_buf_ptr_type and type::IsIntegral(rhs_type)) {
        builder().Store<ir::RegOr<ir::addr_t>>(
            builder().PtrIncr(builder().Load<ir::addr_t>(lhs_lval),
                              out.get<int64_t>(0), lhs_buf_ptr_type),
            lhs_lval);
      } else {
        ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                   uint16_t, uint32_t, uint64_t, float, double>(
            context().qual_types(node->lhs())[0].type(), [&]<typename T>() {
              builder().Store<ir::RegOr<T>>(
                  current_block()->Append(ir::AddInstruction<T>{
                      .lhs    = builder().Load<T>(lhs_lval),
                      .rhs    = builder().CastTo<T>(rhs_type, out[0]),
                      .result = builder().CurrentGroup()->Reserve()}),
                  lhs_lval);
            });
      }
      return;
    } break;
    case frontend::Operator::SubEq: {
      auto lhs_lval       = EmitRef(node->lhs());
      EmitToBuffer(node->rhs(), out);
      type::Type lhs_type = context().qual_types(node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(node->rhs())[0].type();
      if (auto const *lhs_buf_ptr_type = lhs_type.if_as<type::BufferPointer>();
          lhs_buf_ptr_type and type::IsIntegral(rhs_type)) {
        builder().Store<ir::RegOr<ir::addr_t>>(
            builder().PtrIncr(builder().Load<ir::addr_t>(lhs_lval),
                              builder().Neg(out.get<int64_t>(0)),
                              lhs_buf_ptr_type),
            lhs_lval);
      } else {
        ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                   uint16_t, uint32_t, uint64_t, float, double>(
            lhs_type, [&]<typename T>() {
              builder().Store<ir::RegOr<T>>(
                  current_block()->Append(ir::SubInstruction<T>{
                      .lhs    = builder().Load<T>(lhs_lval),
                      .rhs    = builder().CastTo<T>(rhs_type, out[0]),
                      .result = builder().CurrentGroup()->Reserve()}),
                  lhs_lval);
            });
      }
      return;
    } break;
    case frontend::Operator::MulEq: {
      auto lhs_lval = EmitRef(node->lhs());
      EmitToBuffer(node->rhs(), out);
      type::Type lhs_type = context().qual_types(node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(node->rhs())[0].type();
      ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t,
                 uint64_t, float, double>(lhs_type, [&]<typename T>() {
        builder().Store<ir::RegOr<T>>(
            current_block()->Append(ir::MulInstruction<T>{
                .lhs    = builder().Load<T>(lhs_lval),
                .rhs    = builder().CastTo<T>(rhs_type, out[0]),
                .result = builder().CurrentGroup()->Reserve()}),
            lhs_lval);
      });
      return;
    } break;
    case frontend::Operator::DivEq: {
      auto lhs_lval = EmitRef(node->lhs());
      EmitToBuffer(node->rhs(), out);
      type::Type lhs_type = context().qual_types(node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(node->rhs())[0].type();
      ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                 uint16_t, uint32_t, uint64_t, float, double>(
          lhs_type, [&]<typename T>() {
            builder().Store<ir::RegOr<T>>(
                current_block()->Append(ir::DivInstruction<T>{
                    .lhs    = builder().Load<T>(lhs_lval),
                    .rhs    = builder().CastTo<T>(rhs_type, out[0]),
                    .result = builder().CurrentGroup()->Reserve()}),
                lhs_lval);
          });
      return;
    } break;
    case frontend::Operator::ModEq: {
      auto lhs_lval = EmitRef(node->lhs());
      EmitToBuffer(node->rhs(), out);
      type::Type lhs_type = context().qual_types(node->lhs())[0].type();
      type::Type rhs_type = context().qual_types(node->rhs())[0].type();
      ApplyTypes<ir::Integer, int8_t, int16_t, int32_t, int64_t, uint8_t,
                 uint16_t, uint32_t, uint64_t>(lhs_type, [&]<typename T>() {
        builder().Store<ir::RegOr<T>>(
            current_block()->Append(ir::ModInstruction<T>{
                .lhs    = builder().Load<T>(lhs_lval),
                .rhs    = builder().CastTo<T>(rhs_type, out[0]),
                .result = builder().CurrentGroup()->Reserve()}),
            lhs_lval);
      });
      return;
    } break;

    default: { UNREACHABLE(node->DebugString()); } break;
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
    absl::flat_hash_map<ast::Declaration::Id const *, ir::Value> &bindings) {
  std::optional<std::variant<ir::CompleteResultBuffer,
                             std::vector<diagnostic::ConsumedMessage>>>
      lhs_buffer, rhs_buffer;
  if (not node->lhs()->covers_binding()) {
    lhs_buffer = EvaluateToBufferOrDiagnose(
        type::Typed<ast::Expression const *>(node->lhs(), pmc.type));
  }

  if (not node->rhs()->covers_binding()) {
    rhs_buffer = EvaluateToBufferOrDiagnose(
        type::Typed<ast::Expression const *>(node->rhs(), pmc.type));
  }

  switch (node->op()) {
    case frontend::Operator::Add: {
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
        return PatternMatch(node->rhs(), pmc, bindings);
      } else if (p and rhs_buffer) {
        p->Apply([&]<typename T>() {
          auto sum = pmc.value.template get<T>(0);
          auto term = std::get<ir::CompleteResultBuffer>(*rhs_buffer)
                          .template get<T>(0);
          if constexpr (std::is_arithmetic_v<T>) {
            pmc.value.clear();
            pmc.value.append(static_cast<T>(sum - term));
          }
        });
        return PatternMatch(node->lhs(), pmc, bindings);
      } else {
        // TODO: Explain that we cannot match because the pattern is not
        // sufficiently simple.
        diag().Consume(PatternMatchingFailed{.range = node->range()});
        return false;
      }
    } break;
    case frontend::Operator::Sub: {
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
        return PatternMatch(node->rhs(), pmc, bindings);
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
        return PatternMatch(node->lhs(), pmc, bindings);
      } else {
        // TODO: Explain that we cannot match because the pattern is not
        // sufficiently simple.
        diag().Consume(PatternMatchingFailed{.range = node->range()});
      }
    } break;
    case frontend::Operator::Mul: {
      ast::Expression const *expr;
      auto const *p = pmc.type.if_as<type::Primitive>();
      if (p and lhs_buffer) {
        expr = node->rhs();
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
        return PatternMatch(node->rhs(), pmc, bindings);
      } else if (p and rhs_buffer) {
        expr = node->lhs();
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
