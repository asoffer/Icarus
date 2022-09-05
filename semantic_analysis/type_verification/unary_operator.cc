#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/context.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/flags.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/struct.h"

namespace semantic_analysis {
namespace {
using namespace compiler;  // TODO: Remove after migration.

struct UnexpandedUnaryOperatorArgument {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "unexpanded-unary-operator-argument";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Unary operator argument expands to %u values. Each "
                         "operand must expand to exactly 1 value.",
                         num_arguments),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  size_t num_arguments;
  std::string_view view;
};

struct UncopyableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncopyable-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to copy an uncopyable type `%s`.", from),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string from;
  std::string_view view;
};

struct NonConstantInterface {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-interface";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Non-constant type in interface constructor `~`"),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string_view view;
};

struct InvalidUnaryOperatorOverload {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "invalid-unary-operator-overload";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No valid operator overload for (%s)", op),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  char const *op;
  std::string_view view;
};

struct InvalidUnaryOperatorCall {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-unary-operator-call";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Invalid call to unary operator (%s) with argument type `%s`", op,
            type),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  char const *op;
  std::string type;
  std::string_view view;
};

struct NegatingUnsignedInteger {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "negating-unsigned-integer";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to negate an unsigned integer of type `%s`.", type),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string type;
  std::string_view view;
};

struct NonAddressableExpression {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "non-addressable-expression";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expression is not addressable."),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string_view view;
};

struct DereferencingNonPointer {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "dereferencing-non-pointer";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to dereference an object of type `%s` "
                         "which is not a pointer",
                         type),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string type;
  std::string_view view;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::UnaryOperator const *node) {
  absl::Span operand_qts = co_await VerifyTypeOf(node->operand());

  type::QualType qt;

  if (operand_qts.size() != 1) {
    tv.ConsumeDiagnostic(UnexpandedUnaryOperatorArgument{
        .num_arguments = operand_qts.size(),
        .view          = node->operand()->range(),
    });

    qt = type::QualType::Error();
  } else if (not operand_qts[0].ok()) {
    qt = type::QualType::Error();
  } else {
    auto operand_qt   = operand_qts[0];
    auto operand_type = operand_qt.type();

    switch (node->kind()) {
      case ast::UnaryOperator::Kind::Copy: {
        NOT_YET();
      } break;
      case ast::UnaryOperator::Kind::Init: {
        NOT_YET();
      } break;
      case ast::UnaryOperator::Kind::Destroy: {
        NOT_YET();
      } break;
      case ast::UnaryOperator::Kind::Move: {
        NOT_YET();
      } break;
      case ast::UnaryOperator::Kind::BufferPointer: {
        if (operand_type == type::Type_) {
          qt = type::QualType(operand_type,
                              operand_qt.quals() & ~type::Qualifiers::Buffer());
        } else {
          tv.ConsumeDiagnostic(NotAType{
              .view = node->operand()->range(),
              .type = TypeForDiagnostic(node->operand(), tv.context()),
          });
          qt = type::QualType::Error();
        }
      } break;
      case ast::UnaryOperator::Kind::TypeOf: {
        qt = type::QualType::Constant(type::Type_);
      } break;
      case ast::UnaryOperator::Kind::At: {
        if (auto const *ptr_type = operand_type.if_as<type::BufferPointer>()) {
          qt = type::QualType(ptr_type->pointee(), type::Qualifiers::Buffer());
        } else if (auto const *ptr_type = operand_type.if_as<type::Pointer>()) {
          qt = type::QualType(ptr_type->pointee(), type::Qualifiers::Storage());
        } else {
          tv.ConsumeDiagnostic(DereferencingNonPointer{
              .type = TypeForDiagnostic(node->operand(), tv.context()),
              .view = node->range(),
          });
          qt = type::QualType::Error();
        }
      } break;
      case ast::UnaryOperator::Kind::Address: {
        if (operand_qt.quals() >= type::Qualifiers::Buffer()) {
          qt = type::QualType(type::BufPtr(operand_type),
                              type::Qualifiers::Unqualified());
        } else if (operand_qt.quals() >= type::Qualifiers::Storage()) {
          qt = type::QualType(type::Ptr(operand_type),
                              type::Qualifiers::Unqualified());
        } else {
          tv.ConsumeDiagnostic(NonAddressableExpression{.view = node->range()});
          qt = type::QualType::Error();
        }
      } break;
      case ast::UnaryOperator::Kind::Pointer: {
        if (operand_type == type::Type_) {
          qt = type::QualType(operand_type,
                              operand_qt.quals() & ~type::Qualifiers::Buffer());
        } else {
          tv.ConsumeDiagnostic(NotAType{
              .view = node->operand()->range(),
              .type = TypeForDiagnostic(node->operand(), tv.context()),
          });
          qt = type::QualType::Error();
        }
      } break;
      case ast::UnaryOperator::Kind::Negate: {
        if (type::IsSignedNumeric(operand_type)) {
          qt = type::QualType(
              operand_type, operand_qt.quals() & type::Qualifiers::Constant());
        } else if (type::IsUnsignedNumeric(operand_type)) {
          tv.ConsumeDiagnostic(NegatingUnsignedInteger{
              .type = TypeForDiagnostic(node->operand(), tv.context()),
              .view = node->range(),
          });
          qt = type::QualType::Error();
        } else {
          tv.ConsumeDiagnostic(InvalidUnaryOperatorCall{
              .op   = "-",
              .type = TypeForDiagnostic(node->operand(), tv.context()),
              .view = node->range(),
          });
          qt = type::QualType::Error();
        }
      } break;
      case ast::UnaryOperator::Kind::Not: {
        if (operand_type == type::Bool) {
          qt = type::QualType(
              type::Bool, operand_qt.quals() & type::Qualifiers::Constant());
        } else if (operand_type.is<type::Flags>()) {
          qt = type::QualType(
              operand_type, operand_qt.quals() & type::Qualifiers::Constant());
        } else {
          tv.ConsumeDiagnostic(InvalidUnaryOperatorCall{
              .op   = "not",
              .type = TypeForDiagnostic(node->operand(), tv.context()),
              .view = node->range(),
          });
          qt = type::QualType::Error();
        }
      } break;
      case ast::UnaryOperator::Kind::BlockJump: {
        NOT_YET();
      } break;
      default: UNREACHABLE(node->DebugString());
    }
  }
  tv.complete_verification(node, qt);
}

}  // namespace semantic_analysis
