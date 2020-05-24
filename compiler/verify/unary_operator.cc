#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/verify/internal/qual_type_iterator.h"
#include "diagnostic/errors.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct InvalidUnaryOperatorCall {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-unary-operator-call";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Invalid call to unary operator with argument type `%s`",
            type->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type const *type;
  frontend::SourceRange range;
};

struct NegatingUnsignedInteger {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "negating-unsigned-integer";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to negate an unsigned integer of type `%s`.",
            type->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type const *type;
  frontend::SourceRange range;
};

struct WhichNonVariant {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "which-non-variant";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to call `which` an object of type `%s` "
                         "which is not a variant.",
                         type->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type const *type;
  frontend::SourceRange range;
};

struct NonConstantEvaluation {
  static constexpr std::string_view kCategory = "evaluation-error";
  static constexpr std::string_view kName     = "non-constant-evaluation";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot evaluate a non-constant at compile-time."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct NonAddressableExpression {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "non-addressable-expression";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expression is not addressable."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct DereferencingNonPointer {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "dereferencing-non-pointer";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to dereference an object of type `%s` "
                         "which is not a pointer",
                         type->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type const *type;
  frontend::SourceRange range;
};

}  // namespace

type::QualType Compiler::VerifyType(ast::UnaryOperator const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto operand_qt, VerifyType(node->operand()));
  auto *operand_type = operand_qt.type();

  type::QualType qt;

  switch (node->op()) {
    case frontend::Operator::Copy: {
      if (not operand_type->IsCopyable()) {
        diag().Consume(diagnostic::UncopyableType{
            .from  = operand_type,
            .range = node->range(),
        });
      }
      qt = type::QualType(operand_type,
                          operand_qt.quals() & ~type::Quals::Ref());
    } break;
    case frontend::Operator::Move: {
      if (not operand_type->IsMovable()) {
        diag().Consume(diagnostic::ImmovableType{
            .from  = operand_type,
            .range = node->range(),
        });
      }
      qt = type::QualType(operand_type,
                          operand_qt.quals() & ~type::Quals::Ref());
    } break;
    case frontend::Operator::BufPtr: {
      if (operand_type == type::Type_) {
        qt = type::QualType(operand_type,
                            operand_qt.quals() & ~type::Quals::Ref());
      } else {
        diag().Consume(diagnostic::NotAType{
            .range = node->operand()->range(),
            .type  = operand_type,
        });
        return type::QualType::Error();
      }
    } break;
    case frontend::Operator::TypeOf: {
      qt = type::QualType::Constant(type::Type_);
    } break;
    case frontend::Operator::Eval: {
      if (not operand_qt.constant()) {
        // TODO here you could return a correct type and just have there
        // be an error regarding constness. When you do node probably worth a
        // full pass over all verification code.
        diag().Consume(NonConstantEvaluation{
            .range = node->operand()->range(),
        });
        return type::QualType::Error();
      } else {
        qt = type::QualType::Constant(operand_type);
      }
    } break;
    case frontend::Operator::Which: {
      qt = type::QualType::NonConstant(type::Type_);
      if (not operand_type->is<type::Variant>()) {
        diag().Consume(WhichNonVariant{
            .type  = operand_type,
            .range = node->range(),
        });
        qt.MarkError();
      }
    } break;
    case frontend::Operator::At:{
      if (auto const *ptr_type = operand_type->if_as<type::Pointer>()) {
        qt = type::QualType(ptr_type->pointee(), operand_qt.quals());
      } else {
        diag().Consume(DereferencingNonPointer{
            .type  = operand_type,
            .range = node->range(),
        });
        return type::QualType::Error();
      }
    } break;
    case frontend::Operator::And: {
      if (operand_qt.quals() >= type::Quals::Ref()) {
        qt =
            type::QualType(type::Ptr(operand_type), type::Quals::Unqualified());
      } else {
        diag().Consume(NonAddressableExpression{.range = node->range()});
        return type::QualType::Error();
      }
    } break;
    case frontend::Operator::Mul: {
      if (operand_type == type::Type_) {
        qt = type::QualType(operand_type,
                            operand_qt.quals() & ~type::Quals::Ref());
      } else {
        diag().Consume(diagnostic::NotAType{
            .range = node->operand()->range(),
            .type  = operand_type,
        });
        return type::QualType::Error();
      }
    } break;
    case frontend::Operator::Sub: {
      if (type::IsSignedNumeric(operand_type)) {
        qt = type::QualType(operand_type,
                            operand_qt.quals() & type::Quals::Const());
      } else if (type::IsUnsignedNumeric(operand_type)) {
        diag().Consume(NegatingUnsignedInteger{
            .type  = operand_type,
            .range = node->range(),
        });
        return type::QualType::Error();
      } else if (operand_type->is<type::Struct>()) {
        // TODO do you ever want to support overlaods that accepts constants?
        qt = VerifyUnaryOverload("-", node, operand_qt.type());
      } else {
        diag().Consume(InvalidUnaryOperatorCall{
            .type  = operand_type,
            .range = node->range(),
        });
        return type::QualType::Error();
      }
    } break;
    case frontend::Operator::Not: {
      if (operand_type == type::Bool or operand_type->is<type::Flags>()) {
        qt = type::QualType(operand_type,
                            operand_qt.quals() & type::Quals::Const());
      } else if (operand_type->is<type::Struct>()) {
        // TODO do you ever want to support overlaods that accepts constants?
        return VerifyUnaryOverload("-", node, operand_qt.type());
      } else {
        diag().Consume(InvalidUnaryOperatorCall{
            .type  = operand_type,
            .range = node->range(),
        });
        return type::QualType::Error();
      }
    } break;
    default: UNREACHABLE(*node);
  }

  return data().set_qual_type(node, qt);
}

}  // namespace compiler
