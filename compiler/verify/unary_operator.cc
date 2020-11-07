#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/verify/common.h"
#include "compiler/verify/internal/qual_type_iterator.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct UncopyableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncopyable-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to copy an uncopyable type `%s`.",
                         from.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type from;
  frontend::SourceRange range;
};

struct InvalidUnaryOperatorOverload {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "invalid-unary-operator-overload";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No valid operator overload for (%s)"),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  char const *op;
  frontend::SourceRange range;
};

struct InvalidUnaryOperatorCall {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-unary-operator-call";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Invalid call to unary operator (%s) with argument type `%s`", op,
            type.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  char const *op;
  type::Type type;
  frontend::SourceRange range;
};

struct NegatingUnsignedInteger {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "negating-unsigned-integer";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to negate an unsigned integer of type `%s`.",
            type.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type type;
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
                         type.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type type;
  frontend::SourceRange range;
};

}  // namespace

type::QualType Compiler::VerifyType(ast::UnaryOperator const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto operand_qt, VerifyType(node->operand()));
  auto operand_type = operand_qt.type();

  type::QualType qt;

  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Copy: {
      if (not operand_type.get()->IsCopyable()) {
        diag().Consume(UncopyableType{
            .from  = operand_type,
            .range = node->range(),
        });
      }
      qt = type::QualType(operand_type,
                          operand_qt.quals() & ~type::Quals::Buf());
    } break;
    case ast::UnaryOperator::Kind::Init: {
      // TODO: Under what circumstances is `init` allowed?
      qt = type::QualType(operand_type,
                          operand_qt.quals() & ~type::Quals::Buf());
    } break;
    case ast::UnaryOperator::Kind::Move: {
      if (not operand_type.get()->IsMovable()) {
        diag().Consume(ImmovableType{
            .from  = operand_type,
            .range = node->range(),
        });
      }
      qt = type::QualType(operand_type,
                          operand_qt.quals() & ~type::Quals::Buf());
    } break;
    case ast::UnaryOperator::Kind::BufferPointer: {
      if (operand_type == type::Type_) {
        qt = type::QualType(operand_type,
                            operand_qt.quals() & ~type::Quals::Buf());
      } else {
        diag().Consume(NotAType{
            .range = node->operand()->range(),
            .type  = operand_type,
        });
        return type::QualType::Error();
      }
    } break;
    case ast::UnaryOperator::Kind::TypeOf: {
      qt = type::QualType::Constant(type::Type_);
    } break;
    case ast::UnaryOperator::Kind::Evaluate: {
      qt = type::QualType::Constant(operand_type);
      if (not operand_qt.constant()) {
        diag().Consume(NonConstantEvaluation{
            .range = node->operand()->range(),
        });
        qt.MarkError();
      }
    } break;
    case ast::UnaryOperator::Kind::At: {
      if (auto const *ptr_type = operand_type.if_as<type::BufferPointer>()) {
        qt = type::QualType(ptr_type->pointee(), type::Quals::Buf());
      } else if (auto const *ptr_type = operand_type.if_as<type::Pointer>()) {
        qt = type::QualType(ptr_type->pointee(), type::Quals::Ref());
      } else {
        diag().Consume(DereferencingNonPointer{
            .type  = operand_type,
            .range = node->range(),
        });
        return type::QualType::Error();
      }
    } break;
    case ast::UnaryOperator::Kind::Address: {
      if (operand_qt.quals() >= type::Quals::Buf()) {
        qt = type::QualType(type::BufPtr(operand_type),
                            type::Quals::Unqualified());
      } else if (operand_qt.quals() >= type::Quals::Ref()) {
        qt =
            type::QualType(type::Ptr(operand_type), type::Quals::Unqualified());
      } else {
        diag().Consume(NonAddressableExpression{.range = node->range()});
        return type::QualType::Error();
      }
    } break;
    case ast::UnaryOperator::Kind::Pointer: {
      if (operand_type == type::Type_) {
        qt = type::QualType(operand_type,
                            operand_qt.quals() & ~type::Quals::Buf());
      } else {
        diag().Consume(NotAType{
            .range = node->operand()->range(),
            .type  = operand_type,
        });
        return type::QualType::Error();
      }
    } break;
    case ast::UnaryOperator::Kind::Negate: {
      if (type::IsSignedNumeric(operand_type)) {
        qt = type::QualType(operand_type,
                            operand_qt.quals() & type::Quals::Const());
      } else if (type::IsUnsignedNumeric(operand_type)) {
        diag().Consume(NegatingUnsignedInteger{
            .type  = operand_type,
            .range = node->range(),
        });
        return type::QualType::Error();
      } else if (operand_type.is<type::Struct>()) {
        // TODO: support calling with constant arguments.
        qt = VerifyUnaryOverload(
            "-", node, type::Typed<ir::Value>(ir::Value(), operand_qt.type()));
        if (not qt.ok()) {
          diag().Consume(InvalidUnaryOperatorOverload{
              .op    = "-",
              .range = node->range(),
          });
          return type::QualType::Error();
        }
      } else {
        diag().Consume(InvalidUnaryOperatorCall{
            .op    = "-",
            .type  = operand_type,
            .range = node->range(),
        });
        return type::QualType::Error();
      }
    } break;
    case ast::UnaryOperator::Kind::Not: {
      if (operand_type == type::Bool or operand_type.is<type::Flags>()) {
        qt = type::QualType(operand_type,
                            operand_qt.quals() & type::Quals::Const());
      } else if (operand_type.is<type::Struct>()) {
        // TODO: support calling with constant arguments.
        qt = VerifyUnaryOverload(
            "!", node, type::Typed<ir::Value>(ir::Value(), operand_qt.type()));
        if (not qt.ok()) {
          diag().Consume(InvalidUnaryOperatorOverload{
              .op    = "!",
              .range = node->range(),
          });
          return type::QualType::Error();
        }
      } else {
        diag().Consume(InvalidUnaryOperatorCall{
            .op    = "!",
            .type  = operand_type,
            .range = node->range(),
        });
        return type::QualType::Error();
      }
    } break;
    default: UNREACHABLE(*node);
  }

  return context().set_qual_type(node, qt);
}

}  // namespace compiler
