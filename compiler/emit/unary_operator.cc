#include "ast/ast.h"
#include "compiler/compiler.h"
#include "frontend/lex/operators.h"
#include "ir/value/value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::UnaryOperator const *node) {
  // TODO: user-defined-types
  switch (node->op()) {
    case frontend::Operator::Copy: {
      auto *operand_type =
          ASSERT_NOT_NULL(data().qual_type(node->operand()))->type();
      auto reg = builder().TmpAlloca(operand_type);
      EmitCopyInit(type::Typed(EmitValue(node->operand()), operand_type),
                   type::Typed<ir::Reg>(reg, type::Ptr(operand_type)));
      return ir::Value(builder().PtrFix(reg, operand_type));
    } break;
    case frontend::Operator::Move: {
      auto *operand_type =
          ASSERT_NOT_NULL(data().qual_type(node->operand()))->type();
      auto reg = builder().TmpAlloca(operand_type);
      EmitMoveInit(type::Typed(EmitValue(node->operand()), operand_type),
                   type::Typed<ir::Reg>(reg, type::Ptr(operand_type)));
      return ir::Value(builder().PtrFix(reg, operand_type));
    } break;
    case frontend::Operator::BufPtr:
      return ir::Value(builder().BufPtr(
          EmitValue(node->operand()).get<ir::RegOr<type::Type const *>>()));
    case frontend::Operator::Not: {
      auto *t = ASSERT_NOT_NULL(data().qual_type(node->operand()))->type();
      if (t == type::Bool) {
        return ir::Value(
            builder().Not(EmitValue(node->operand()).get<ir::RegOr<bool>>()));
      } else {
        return ir::Value(builder().XorFlags(
            EmitValue(node->operand()).get<ir::RegOr<ir::FlagsVal>>(),
            ir::FlagsVal{t->as<type::Flags>().All}));
      }
    } break;
    case frontend::Operator::Sub: {
      auto operand_ir = EmitValue(node->operand());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, float, double>(
          ASSERT_NOT_NULL(data().qual_type(node->operand()))->type(),
          [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Neg(operand_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case frontend::Operator::TypeOf:
      return ir::Value(
          ASSERT_NOT_NULL(data().qual_type(node->operand()))->type());
    case frontend::Operator::Which:
      return ir::Value(builder().Load<type::Type const *>(
          builder().VariantType(EmitValue(node->operand()).get<ir::Reg>())));
    case frontend::Operator::And: return ir::Value(EmitRef(node->operand()));
    case frontend::Operator::Eval: {
      // TODO: There's a chance this was already computed, in which case we
      // should not execute it more than once. For example, if it was used in a
      // context where evaluation was implicit. 
      // ```
      // n: `int64
      // ```
      auto maybe_val = Evaluate(type::Typed(
          node->operand(),
          ASSERT_NOT_NULL(data().qual_type(node->operand()))->type()));
      if (not maybe_val) {
        diag().Consume(diagnostic::EvaluationFailure{
            .failure = maybe_val.error(),
            .range   = node->operand()->range(),
        });
        return ir::Value();
      }
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
      return builder().Load(
          EmitValue(node->operand()).get<ir::RegOr<ir::Addr>>(),
          ASSERT_NOT_NULL(data().qual_type(node))->type());
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
      EmitCopyInit(type::Typed(EmitValue(node),
                               ASSERT_NOT_NULL(data().qual_type(node))->type()),
                   reg);
      break;
  }
}

void Compiler::EmitMoveInit(ast::UnaryOperator const *node,
                            type::Typed<ir::Reg> reg) {
  switch (node->op()) {
    case frontend::Operator::Move: EmitMoveInit(node->operand(), reg); break;
    case frontend::Operator::Copy: EmitCopyInit(node->operand(), reg); break;
    default:
      EmitMoveInit(type::Typed(EmitValue(node),
                               ASSERT_NOT_NULL(data().qual_type(node))->type()),
                   reg);
      break;
  }
}

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::UnaryOperator const *node) {
  ASSERT(node->op() == frontend::Operator::At);
  return EmitValue(node->operand()).get<ir::Reg>();
}

}  // namespace compiler
