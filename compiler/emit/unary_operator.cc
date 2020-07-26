#include "ast/ast.h"
#include "compiler/compiler.h"
#include "frontend/lex/operators.h"
#include "ir/value/value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::UnaryOperator const *node) {
  // TODO: user-defined-types
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Copy: {
      auto *operand_type =
          ASSERT_NOT_NULL(data().qual_type(node->operand()))->type();
      auto reg = builder().TmpAlloca(operand_type);
      EmitCopyInit(type::Typed(EmitValue(node->operand()), operand_type),
                   type::Typed<ir::Reg>(reg, type::Ptr(operand_type)));
      return ir::Value(builder().PtrFix(reg, operand_type));
    } break;
    case ast::UnaryOperator::Kind::Init:
      // TODO: Not entirely sure this is what the semantics ought to be.
    case ast::UnaryOperator::Kind::Move: {
      auto *operand_type =
          ASSERT_NOT_NULL(data().qual_type(node->operand()))->type();
      auto reg = builder().TmpAlloca(operand_type);
      EmitMoveInit(type::Typed(EmitValue(node->operand()), operand_type),
                   type::Typed<ir::Reg>(reg, type::Ptr(operand_type)));
      return ir::Value(builder().PtrFix(reg, operand_type));
      } break;
    case ast::UnaryOperator::Kind::BufferPointer:
      return ir::Value(builder().BufPtr(
          EmitValue(node->operand()).get<ir::RegOr<type::Type const *>>()));
    case ast::UnaryOperator::Kind::Not: {
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
    case ast::UnaryOperator::Kind::Negate: {
      auto operand_ir = EmitValue(node->operand());
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, float, double>(
          ASSERT_NOT_NULL(data().qual_type(node->operand()))->type(),
          [&](auto tag) {
            using T = typename decltype(tag)::type;
            return ir::Value(builder().Neg(operand_ir.get<ir::RegOr<T>>()));
          });
    } break;
    case ast::UnaryOperator::Kind::TypeOf:
      return ir::Value(
          ASSERT_NOT_NULL(data().qual_type(node->operand()))->type());
    case ast::UnaryOperator::Kind::Which:
      return ir::Value(builder().Load<type::Type const *>(
          builder().VariantType(EmitValue(node->operand()).get<ir::Reg>())));
    case ast::UnaryOperator::Kind::Address: return ir::Value(EmitRef(node->operand()));
    case ast::UnaryOperator::Kind::Evaluate: {
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
    case ast::UnaryOperator::Kind::Pointer: {
      state_.must_complete = false;

      ir::Value value(builder().Ptr(
          EmitValue(node->operand()).get<ir::RegOr<type::Type const *>>()));

      state_.must_complete = true;

      return value;
    } break;
    case ast::UnaryOperator::Kind::At: {
      return builder().Load(
          EmitValue(node->operand()).get<ir::RegOr<ir::Addr>>(),
          ASSERT_NOT_NULL(data().qual_type(node))->type());
    }
    default: UNREACHABLE("Operator is ", static_cast<int>(node->kind()));
  }
}

void Compiler::EmitCopyInit(
    ast::UnaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Init:
      EmitCopyInit(node->operand(), to);
      break;
    case ast::UnaryOperator::Kind::Move:
      EmitMoveInit(node->operand(), to);
      break;
    case ast::UnaryOperator::Kind::Copy:
      EmitCopyInit(node->operand(), to);
      break;
    default: {
      auto from_val = EmitValue(node);
      auto from_qt  = *data().qual_type(node);
      if (to.size() == 1) {
        Visit(to[0].type()->as<type::Pointer>().pointee(), *to[0],
              type::Typed{from_val, from_qt.type()}, EmitCopyAssignTag{});
      } else {
        auto val_iter = from_val.get<ir::MultiValue>().span().begin();
        auto ref_iter = to.begin();
        for (auto *t : from_qt.expanded()) {
          type::Typed<ir::RegOr<ir::Addr>> ref = *ref_iter++;
          Visit(ref.type()->as<type::Pointer>().pointee(), *ref,
                type::Typed{*val_iter++, t}, EmitCopyAssignTag{});
        }
      }
    } break;
  }
}

void Compiler::EmitMoveInit(
    ast::UnaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Init:
      EmitMoveInit(node->operand(), to);
      break;
    case ast::UnaryOperator::Kind::Move:
      EmitMoveInit(node->operand(), to);
      break;
    case ast::UnaryOperator::Kind::Copy:
      EmitCopyInit(node->operand(), to);
      break;
    default: {
      auto from_val = EmitValue(node);
      auto from_qt  = *data().qual_type(node);
      if (to.size() == 1) {
        Visit(to[0].type()->as<type::Pointer>().pointee(), *to[0],
              type::Typed{from_val, from_qt.type()}, EmitMoveAssignTag{});
      } else {
        auto val_iter = from_val.get<ir::MultiValue>().span().begin();
        auto ref_iter = to.begin();
        for (auto *t : from_qt.expanded()) {
          type::Typed<ir::RegOr<ir::Addr>> ref = *ref_iter++;
          Visit(ref.type()->as<type::Pointer>().pointee(), *ref,
                type::Typed{*val_iter++, t}, EmitMoveAssignTag{});
        }
      }
    } break;
  }
}

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::UnaryOperator const *node) {
  ASSERT(node->kind() == ast::UnaryOperator::Kind::At);
  return EmitValue(node->operand()).get<ir::Reg>();
}

// TODO: Unit tests
void Compiler::EmitAssign(
    ast::UnaryOperator const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Init: {
      EmitMoveInit(node->operand(), to);
    } break;
    case ast::UnaryOperator::Kind::Copy: {
      auto operand_qt = *ASSERT_NOT_NULL(data().qual_type(node->operand()));
      std::vector<type::Typed<ir::RegOr<ir::Addr>>> tmps;
      operand_qt.ForEach([&](type::Type const *t) {
        tmps.emplace_back(ir::RegOr<ir::Addr>(builder().TmpAlloca(t)), t);
      });
      NOT_YET();
    } break;
    case ast::UnaryOperator::Kind::Move: EmitAssign(node->operand(), to); break;
    default: {
      auto from_val = EmitValue(node);
      auto from_qt  = *data().qual_type(node);
      if (to.size() == 1) {
        Visit(to[0].type()->as<type::Pointer>().pointee(), *to[0],
              type::Typed{from_val, from_qt.type()}, EmitMoveAssignTag{});
      } else {
        auto val_iter = from_val.get<ir::MultiValue>().span().begin();
        auto ref_iter = to.begin();
        for (auto *t : from_qt.expanded()) {
          type::Typed<ir::RegOr<ir::Addr>> ref = *ref_iter++;
          Visit(ref.type(), *ref, type::Typed{*val_iter++, t},
                EmitMoveAssignTag{});
        }
      }
    } break;
  }
}

}  // namespace compiler
