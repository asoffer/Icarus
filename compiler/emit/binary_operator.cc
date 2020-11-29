#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::BinaryOperator const *node) {
  switch (node->op()) {
    case frontend::Operator::Or: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      auto t      = ASSERT_NOT_NULL(context().qual_type(node))->type();
      if (t == type::Bool) {
        auto *land_block = builder().AddBlock();

        std::vector<ir::BasicBlock const *> phi_blocks;

        auto *next_block = builder().AddBlock();
        builder().CondJump(lhs_ir.get<ir::RegOr<bool>>(), land_block,
                           next_block);
        phi_blocks.push_back(builder().CurrentBlock());
        builder().CurrentBlock() = next_block;

        auto rhs_ir = EmitValue(node->rhs());
        phi_blocks.push_back(builder().CurrentBlock());
        builder().UncondJump(land_block);

        builder().CurrentBlock() = land_block;

        return ir::Value(builder().Phi<bool>(
            std::move(phi_blocks), {true, rhs_ir.get<ir::RegOr<bool>>()}));
      } else if (t.is<type::Flags>()) {
        // `|` is not overloadable, and blocks piped together must be done
        // syntactically in a `goto` node and are handled by the parser.
        return ir::Value(current_block()->Append(type::OrFlagsInstruction{
            .lhs    = lhs_ir.get<ir::RegOr<type::Flags::underlying_type>>(),
            .rhs    = rhs_ir.get<ir::RegOr<type::Flags::underlying_type>>(),
            .result = builder().CurrentGroup()->Reserve()}));
      } else {
        UNREACHABLE();
      }
    } break;
    case frontend::Operator::Xor: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      auto t      = ASSERT_NOT_NULL(context().qual_type(node))->type();
      if (t == type::Bool) {
        return ir::Value(builder().Ne(lhs_ir.get<ir::RegOr<bool>>(),
                                      rhs_ir.get<ir::RegOr<bool>>()));
      } else if (t.is<type::Flags>()) {
        return ir::Value(current_block()->Append(type::XorFlagsInstruction{
            .lhs    = lhs_ir.get<ir::RegOr<type::Flags::underlying_type>>(),
            .rhs    = rhs_ir.get<ir::RegOr<type::Flags::underlying_type>>(),
            .result = builder().CurrentGroup()->Reserve()}));
      } else {
        UNREACHABLE();
      }
    } break;
    case frontend::Operator::And: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      auto t      = ASSERT_NOT_NULL(context().qual_type(node))->type();
      if (t == type::Bool) {
        auto *land_block = builder().AddBlock();

        std::vector<ir::BasicBlock const *> phi_blocks;

        auto *next_block = builder().AddBlock();
        builder().CondJump(lhs_ir.get<ir::RegOr<bool>>(), next_block,
                           land_block);
        phi_blocks.push_back(builder().CurrentBlock());
        builder().CurrentBlock() = next_block;

        auto rhs_ir = EmitValue(node->rhs());
        phi_blocks.push_back(builder().CurrentBlock());
        builder().UncondJump(land_block);

        builder().CurrentBlock() = land_block;

        return ir::Value(builder().Phi<bool>(
            std::move(phi_blocks), {false, rhs_ir.get<ir::RegOr<bool>>()}));
      } else if (t.is<type::Flags>()) {
        // `|` is not overloadable, and blocks piped together must be done
        // syntactically in a `goto` node and are handled by the parser.
        return ir::Value(current_block()->Append(type::AndFlagsInstruction{
            .lhs    = lhs_ir.get<ir::RegOr<type::Flags::underlying_type>>(),
            .rhs    = rhs_ir.get<ir::RegOr<type::Flags::underlying_type>>(),
            .result = builder().CurrentGroup()->Reserve()}));
      } else {
        UNREACHABLE();
      }
    } break;
    case frontend::Operator::Add: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          ASSERT_NOT_NULL(context().qual_type(node->lhs()))->type(),
          [&]<typename T>() {
            return ir::Value(current_block()->Append(ir::AddInstruction<T>{
                .lhs    = lhs_ir.get<ir::RegOr<T>>(),
                .rhs    = rhs_ir.get<ir::RegOr<T>>(),
                .result = builder().CurrentGroup()->Reserve()}));
          });
    } break;
    case frontend::Operator::Sub: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          ASSERT_NOT_NULL(context().qual_type(node->lhs()))->type(),
          [&]<typename T>() {
            return ir::Value(current_block()->Append(ir::SubInstruction<T>{
                .lhs    = lhs_ir.get<ir::RegOr<T>>(),
                .rhs    = rhs_ir.get<ir::RegOr<T>>(),
                .result = builder().CurrentGroup()->Reserve()}));
          });
    } break;
    case frontend::Operator::Mul: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          ASSERT_NOT_NULL(context().qual_type(node->lhs()))->type(),
          [&]<typename T>() {
            return ir::Value(current_block()->Append(ir::MulInstruction<T>{
                .lhs    = lhs_ir.get<ir::RegOr<T>>(),
                .rhs    = rhs_ir.get<ir::RegOr<T>>(),
                .result = builder().CurrentGroup()->Reserve()}));
          });
    } break;
    case frontend::Operator::Div: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          ASSERT_NOT_NULL(context().qual_type(node->lhs()))->type(),
          [&]<typename T>() {
            return ir::Value(current_block()->Append(ir::DivInstruction<T>{
                .lhs    = lhs_ir.get<ir::RegOr<T>>(),
                .rhs    = rhs_ir.get<ir::RegOr<T>>(),
                .result = builder().CurrentGroup()->Reserve()}));
          });
    } break;
    case frontend::Operator::Mod: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t>(
          ASSERT_NOT_NULL(context().qual_type(node->lhs()))->type(),
          [&]<typename T>() {
            return ir::Value(current_block()->Append(ir::ModInstruction<T>{
                .lhs    = lhs_ir.get<ir::RegOr<T>>(),
                .rhs    = rhs_ir.get<ir::RegOr<T>>(),
                .result = builder().CurrentGroup()->Reserve()}));
          });
    } break;
    case frontend::Operator::OrEq: {
      auto this_type = ASSERT_NOT_NULL(context().qual_type(node))->type();
      auto lhs_lval  = EmitRef(node->lhs());
      if (this_type == type::Bool) {
        auto *land_block = builder().AddBlock();
        auto *more_block = builder().AddBlock();

        auto lhs_val       = builder().Load<bool>(lhs_lval);
        auto lhs_end_block = builder().CurrentBlock();
        builder().CondJump(lhs_val, land_block, more_block);

        builder().CurrentBlock() = more_block;
        auto rhs_val       = EmitValue(node->rhs()).get<ir::RegOr<bool>>();
        auto rhs_end_block = builder().CurrentBlock();
        builder().UncondJump(land_block);

        builder().CurrentBlock() = land_block;
        builder().Store(builder().Phi<bool>({lhs_end_block, rhs_end_block},
                                            {ir::RegOr<bool>(true), rhs_val}),
                        lhs_lval);
      } else if (this_type.is<type::Flags>()) {
        builder().Store<ir::RegOr<type::Flags::underlying_type>>(
            current_block()->Append(type::OrFlagsInstruction{
                .lhs = builder().Load<type::Flags::underlying_type>(lhs_lval),
                .rhs = EmitValue(node->rhs())
                           .get<ir::RegOr<type::Flags::underlying_type>>(),
                .result = builder().CurrentGroup()->Reserve()}),
            lhs_lval);
      } else {
        UNREACHABLE(this_type.to_string());
      }
      return ir::Value();
    } break;
    case frontend::Operator::AndEq: {
      auto this_type = ASSERT_NOT_NULL(context().qual_type(node))->type();
      auto lhs_lval  = EmitRef(node->lhs());
      if (this_type.is<type::Flags>()) {
        builder().Store<ir::RegOr<type::Flags::underlying_type>>(
            current_block()->Append(type::AndFlagsInstruction{
                .lhs = builder().Load<type::Flags::underlying_type>(lhs_lval),
                .rhs = EmitValue(node->rhs())
                           .get<ir::RegOr<type::Flags::underlying_type>>(),
                .result = builder().CurrentGroup()->Reserve()}),
            lhs_lval);
      } else if (this_type == type::Bool) {
        auto *land_block = builder().AddBlock();
        auto *more_block = builder().AddBlock();

        auto lhs_val       = builder().Load<bool>(lhs_lval);
        auto lhs_end_block = builder().CurrentBlock();
        builder().CondJump(lhs_val, more_block, land_block);

        builder().CurrentBlock() = more_block;
        auto rhs_val       = EmitValue(node->rhs()).get<ir::RegOr<bool>>();
        auto rhs_end_block = builder().CurrentBlock();
        builder().UncondJump(land_block);

        builder().CurrentBlock() = land_block;
        builder().Store(builder().Phi<bool>({lhs_end_block, rhs_end_block},
                                            {ir::RegOr<bool>(false), rhs_val}),
                        lhs_lval);
      } else {
        UNREACHABLE(this_type.to_string());
      }
      return ir::Value();
    } break;
    case frontend::Operator::XorEq: {
      auto this_type = ASSERT_NOT_NULL(context().qual_type(node))->type();
      auto lhs_lval  = EmitRef(node->lhs());
      if (this_type.is<type::Flags>()) {
        auto rhs_ir = EmitValue(node->rhs())
                          .get<ir::RegOr<type::Flags::underlying_type>>();
        builder().Store<ir::RegOr<type::Flags::underlying_type>>(
            current_block()->Append(type::XorFlagsInstruction{
                .lhs = builder().Load<type::Flags::underlying_type>(lhs_lval),
                .rhs = rhs_ir,
                .result = builder().CurrentGroup()->Reserve()}),
            lhs_lval);
      } else if (this_type == type::Bool) {
        auto rhs_ir = EmitValue(node->rhs()).get<ir::RegOr<bool>>();
        builder().Store(builder().Ne(builder().Load<bool>(lhs_lval), rhs_ir),
                        lhs_lval);
      } else {
        UNREACHABLE(this_type.to_string());
      }
      return ir::Value();
    } break;
    case frontend::Operator::AddEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          ASSERT_NOT_NULL(context().qual_type(node->lhs()))->type(),
          [&]<typename T>() {
            builder().Store<ir::RegOr<T>>(
                current_block()->Append(ir::AddInstruction<T>{
                    .lhs    = builder().Load<T>(lhs_lval),
                    .rhs    = rhs_ir.get<ir::RegOr<T>>(),
                    .result = builder().CurrentGroup()->Reserve()}),
                lhs_lval);
          });
      return ir::Value();
    } break;
    case frontend::Operator::SubEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          ASSERT_NOT_NULL(context().qual_type(node->lhs()))->type(),
          [&]<typename T>() {
            builder().Store<ir::RegOr<T>>(
                current_block()->Append(ir::SubInstruction<T>{
                    .lhs    = builder().Load<T>(lhs_lval),
                    .rhs    = rhs_ir.get<ir::RegOr<T>>(),
                    .result = builder().CurrentGroup()->Reserve()}),
                lhs_lval);
          });
      return ir::Value();
    } break;
    case frontend::Operator::MulEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          ASSERT_NOT_NULL(context().qual_type(node->lhs()))->type(),
          [&]<typename T>() {
            builder().Store<ir::RegOr<T>>(
                current_block()->Append(ir::MulInstruction<T>{
                    .lhs    = builder().Load<T>(lhs_lval),
                    .rhs    = rhs_ir.get<ir::RegOr<T>>(),
                    .result = builder().CurrentGroup()->Reserve()}),
                lhs_lval);
          });
      return ir::Value();
    } break;
    case frontend::Operator::DivEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          ASSERT_NOT_NULL(context().qual_type(node->lhs()))->type(),
          [&]<typename T>() {
            builder().Store<ir::RegOr<T>>(
                current_block()->Append(ir::DivInstruction<T>{
                    .lhs    = builder().Load<T>(lhs_lval),
                    .rhs    = rhs_ir.get<ir::RegOr<T>>(),
                    .result = builder().CurrentGroup()->Reserve()}),
                lhs_lval);
          });
      return ir::Value();
    } break;
    case frontend::Operator::ModEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t>(
          ASSERT_NOT_NULL(context().qual_type(node->lhs()))->type(),
          [&]<typename T>() {
            builder().Store<ir::RegOr<T>>(
                current_block()->Append(ir::ModInstruction<T>{
                    .lhs    = builder().Load<T>(lhs_lval),
                    .rhs    = rhs_ir.get<ir::RegOr<T>>(),
                    .result = builder().CurrentGroup()->Reserve()}),
                lhs_lval);
          });
      return ir::Value();
    } break;

    default: { UNREACHABLE(node->DebugString()); } break;
  }
}

void Compiler::EmitCopyInit(
    ast::BinaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_type(node)->type();
  EmitCopyAssign(type::Typed<ir::RegOr<ir::Addr>>(*to[0], t),
                 type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitMoveInit(
    ast::BinaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_type(node)->type();
  EmitMoveAssign(type::Typed<ir::RegOr<ir::Addr>>(*to[0], t),
                 type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitAssign(
    ast::BinaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_type(node)->type();
  EmitCopyAssign(type::Typed<ir::RegOr<ir::Addr>>(*to[0], t),
                 type::Typed<ir::Value>(EmitValue(node), t));
}

}  // namespace compiler
