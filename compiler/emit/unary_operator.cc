#include "ast/ast.h"
#include "compiler/compiler.h"
#include "frontend/lex/operators.h"
#include "ir/value/value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::UnaryOperator const *node) {
  auto *operand_type = type_of(node->operand());
  // TODO user-defined-types

  switch (node->op()) {
    case frontend::Operator::Copy: {
      auto reg = builder().TmpAlloca(operand_type);
      EmitCopyInit(type::Typed(EmitValue(node->operand()), operand_type),
                   type::Typed<ir::Reg>(reg, type::Ptr(operand_type)));
      return ir::Value(builder().PtrFix(reg, operand_type));
    } break;
    case frontend::Operator::Move: {
      auto reg = builder().TmpAlloca(operand_type);
      EmitMoveInit(type::Typed(EmitValue(node->operand()), operand_type),
                   type::Typed<ir::Reg>(reg, type::Ptr(operand_type)));
      return ir::Value(builder().PtrFix(reg, operand_type));
    } break;
    case frontend::Operator::BufPtr:
      return ir::Value(builder().BufPtr(
          EmitValue(node->operand()).get<ir::RegOr<type::Type const *>>()));
    case frontend::Operator::Not: {
      auto *t = type_of(node->operand());
      if (t == type::Bool) {
        return ir::Value(
            builder().Not(EmitValue(node->operand()).get<ir::RegOr<bool>>()));
      } else if (t->is<type::Flags>()) {
        return ir::Value(builder().XorFlags(
            EmitValue(node->operand()).get<ir::RegOr<ir::FlagsVal>>(),
            ir::FlagsVal{t->as<type::Flags>().All}));

      } else {
        NOT_YET();
      }
    } break;
    case frontend::Operator::Sub: {
      auto operand_ir = EmitValue(node->operand());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, float, double>(
          type_of(node->operand()), [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Neg(operand_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case frontend::Operator::TypeOf: return ir::Value(type_of(node->operand()));
    case frontend::Operator::Which:
      return ir::Value(builder().Load<type::Type const *>(
          builder().VariantType(EmitValue(node->operand()).get<ir::Reg>())));
    case frontend::Operator::And: return ir::Value(EmitRef(node->operand()));
    case frontend::Operator::Eval: {
      // Guaranteed to be constant by VerifyType
      auto maybe_val =
          Evaluate(type::Typed(node->operand(), type_of(node->operand())));
      if (not maybe_val) { NOT_YET(); }
      return *maybe_val;
    }
    case frontend::Operator::Mul: {
      state_.must_complete = false;

      ir::Value value(builder().Ptr(
          EmitValue(node->operand()).get<ir::RegOr<type::Type const *>>()));

      state_.must_complete = true;

      return value;
    } break;
    case frontend::Operator::At: {
      auto *t = type_of(node);
      return builder().Load(
          EmitValue(node->operand()).get<ir::RegOr<ir::Addr>>(), t);
    }
    default: UNREACHABLE("Operator is ", static_cast<int>(node->op()));
  }
}

void Compiler::EmitCopyInit(ast::UnaryOperator const *node,
                            type::Typed<ir::Reg> reg) {
  switch (node->op()) {
    case frontend::Operator::Move: EmitMoveInit(node->operand(), reg); break;
    case frontend::Operator::Copy: EmitCopyInit(node->operand(), reg); break;
    default:
      EmitCopyInit(type::Typed(EmitValue(node), type_of(node)), reg);
      break;
  }
}

void Compiler::EmitMoveInit(ast::UnaryOperator const *node,
                            type::Typed<ir::Reg> reg) {
  switch (node->op()) {
    case frontend::Operator::Move: EmitMoveInit(node->operand(), reg); break;
    case frontend::Operator::Copy: EmitCopyInit(node->operand(), reg); break;
    default:
      EmitMoveInit(type::Typed(EmitValue(node), type_of(node)), reg);
      break;
  }
}

}  // namespace compiler
