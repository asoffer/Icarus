#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::BinaryOperator const *node) {
  switch (node->op()) {
    case frontend::Operator::Or: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      auto const *t = ASSERT_NOT_NULL(data().qual_type(node))->type();
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
      } else if (t->is<type::Flags>()) {
        // `|` is not overloadable, and blocks piped together must be done
        // syntactically in a `goto` node and are handled by the parser.
        return ir::Value(
            builder().OrFlags(lhs_ir.get<ir::RegOr<ir::FlagsVal>>(),
                              rhs_ir.get<ir::RegOr<ir::FlagsVal>>()));
      } else {
        UNREACHABLE();
      }
    } break;
    case frontend::Operator::Xor: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      auto const *t = ASSERT_NOT_NULL(data().qual_type(node))->type();
      if (t == type::Bool) {
        return ir::Value(builder().Ne(lhs_ir.get<ir::RegOr<bool>>(),
                                      rhs_ir.get<ir::RegOr<bool>>()));
      } else if (t->is<type::Flags>()) {
        return ir::Value(
            builder().XorFlags(lhs_ir.get<ir::RegOr<ir::FlagsVal>>(),
                               rhs_ir.get<ir::RegOr<ir::FlagsVal>>()));
      } else {
        UNREACHABLE();
      }
    } break;
    case frontend::Operator::And: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      auto const *t = ASSERT_NOT_NULL(data().qual_type(node))->type();
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
      } else if (t->is<type::Flags>()) {
        // `|` is not overloadable, and blocks piped together must be done
        // syntactically in a `goto` node and are handled by the parser.
        return ir::Value(
            builder().AndFlags(lhs_ir.get<ir::RegOr<ir::FlagsVal>>(),
                               rhs_ir.get<ir::RegOr<ir::FlagsVal>>()));
      } else {
        UNREACHABLE();
      }
    } break;
    case frontend::Operator::Add: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          ASSERT_NOT_NULL(data().qual_type(node->lhs()))->type(),
          [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Add(lhs_ir.get<ir::RegOr<T>>(),
                                           rhs_ir.get<ir::RegOr<T>>()));
          });
    } break;
    default: break;
  }

  // TODO: Nothing below here has been tested.
  auto *lhs_qt   = data().qual_type(node->lhs());
  auto *rhs_qt   = data().qual_type(node->rhs());

  auto *lhs_type = ASSERT_NOT_NULL(lhs_qt)->type();
  auto *rhs_type = ASSERT_NOT_NULL(lhs_qt)->type();

  switch (node->op()) {
    case frontend::Operator::Sub: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Sub(lhs_ir.get<ir::RegOr<T>>(),
                                           rhs_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case frontend::Operator::Mul: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Mul(lhs_ir.get<ir::RegOr<T>>(),
                                           rhs_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case frontend::Operator::Div: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Div(lhs_ir.get<ir::RegOr<T>>(),
                                           rhs_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case frontend::Operator::Mod: {
      auto lhs_ir = EmitValue(node->lhs());
      auto rhs_ir = EmitValue(node->rhs());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Mod(lhs_ir.get<ir::RegOr<T>>(),
                                           rhs_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case frontend::Operator::OrEq: {
      auto *this_type = type_of(node);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = EmitRef(node->lhs());
        builder().Store(
            builder().OrFlags(
                builder().Load<ir::FlagsVal>(lhs_lval),
                EmitValue(node->rhs()).get<ir::RegOr<ir::FlagsVal>>()),
            lhs_lval);
        return ir::Value();
      }
      auto *land_block = builder().AddBlock();
      auto *more_block = builder().AddBlock();

      auto lhs_val       = EmitValue(node->lhs()).get<ir::RegOr<bool>>();
      auto lhs_end_block = builder().CurrentBlock();
      builder().CondJump(lhs_val, land_block, more_block);

      builder().CurrentBlock() = more_block;
      auto rhs_val             = EmitValue(node->rhs()).get<ir::RegOr<bool>>();
      auto rhs_end_block       = builder().CurrentBlock();
      builder().UncondJump(land_block);

      builder().CurrentBlock() = land_block;

      return ir::Value(builder().Phi<bool>({lhs_end_block, rhs_end_block},
                                           {ir::RegOr<bool>(true), rhs_val}));
    } break;
    case frontend::Operator::AndEq: {
      auto *this_type = type_of(node);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = EmitRef(node->lhs());
        builder().Store(
            builder().AndFlags(
                builder().Load<ir::FlagsVal>(lhs_lval),
                EmitValue(node->rhs()).get<ir::RegOr<ir::FlagsVal>>()),
            lhs_lval);
        return ir::Value();
      }

      auto *land_block = builder().AddBlock();
      auto *more_block = builder().AddBlock();

      auto lhs_val       = EmitValue(node->lhs()).get<ir::RegOr<bool>>();
      auto lhs_end_block = builder().CurrentBlock();
      builder().CondJump(lhs_val, more_block, land_block);

      builder().CurrentBlock() = more_block;
      auto rhs_val             = EmitValue(node->rhs()).get<ir::RegOr<bool>>();
      auto rhs_end_block       = builder().CurrentBlock();
      builder().UncondJump(land_block);

      builder().CurrentBlock() = land_block;

      // TODO this looks like a bug.
      return ir::Value(builder().Phi<bool>({lhs_end_block, rhs_end_block},
                                           {rhs_val, ir::RegOr<bool>(false)}));
    } break;
    case frontend::Operator::AddEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            builder().Store(builder().Add(builder().Load<T>(lhs_lval),
                                          rhs_ir.get<ir::RegOr<T>>()),
                            lhs_lval);
          });
      return ir::Value();
    } break;
    case frontend::Operator::SubEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            builder().Store(builder().Sub(builder().Load<T>(lhs_lval),
                                          rhs_ir.get<ir::RegOr<T>>()),
                            lhs_lval);
          });
      return ir::Value();
    } break;
    case frontend::Operator::DivEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            builder().Store(builder().Div(builder().Load<T>(lhs_lval),
                                          rhs_ir.get<ir::RegOr<T>>()),
                            lhs_lval);
          });
      return ir::Value();
    } break;
    case frontend::Operator::ModEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t>(rhs_type, [&](auto tag) {
        using T = typename decltype(tag)::type;
        builder().Store(builder().Div(builder().Load<T>(lhs_lval),
                                      rhs_ir.get<ir::RegOr<T>>()),
                        lhs_lval);
      });
      return ir::Value();
    } break;
    case frontend::Operator::MulEq: {
      auto lhs_lval = EmitRef(node->lhs());
      auto rhs_ir   = EmitValue(node->rhs());
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto tag) {
            using T = typename decltype(tag)::type;
            builder().Store(builder().Mul(builder().Load<T>(lhs_lval),
                                          rhs_ir.get<ir::RegOr<T>>()),
                            lhs_lval);
          });
      return ir::Value();
    } break;
    case frontend::Operator::XorEq: {
      if (lhs_type == type::Bool) {
        auto lhs_lval = EmitRef(node->lhs());
        auto rhs_ir   = EmitValue(node->rhs()).get<ir::RegOr<bool>>();
        builder().Store(builder().Ne(builder().Load<bool>(lhs_lval), rhs_ir),
                        lhs_lval);
      } else if (lhs_type->is<type::Flags>()) {
        auto *flags_type = &lhs_type->as<type::Flags>();
        auto lhs_lval    = EmitRef(node->lhs());
        auto rhs_ir = EmitValue(node->rhs()).get<ir::RegOr<ir::FlagsVal>>();
        builder().Store(
            builder().XorFlags(builder().Load<ir::FlagsVal>(lhs_lval), rhs_ir),
            lhs_lval);
      } else {
        UNREACHABLE(lhs_type);
      }
      return ir::Value();
    } break;
    default: UNREACHABLE(*node);
  }
  UNREACHABLE(*node);
}

void Compiler::EmitCopyInit(
    ast::BinaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = data().qual_type(node)->type();
  Visit(t, *to[0], type::Typed{EmitValue(node), t}, EmitCopyAssignTag{});
}

void Compiler::EmitMoveInit(
    ast::BinaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = data().qual_type(node)->type();
  Visit(t, *to[0], type::Typed{EmitValue(node), t}, EmitMoveAssignTag{});
}

void Compiler::EmitAssign(
    ast::BinaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = data().qual_type(node)->type();
  Visit(t, *to[0], type::Typed{EmitValue(node), t}, EmitCopyAssignTag{});
}



}  // namespace compiler
