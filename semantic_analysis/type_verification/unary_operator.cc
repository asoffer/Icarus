#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/common_diagnostics.h"
#include "semantic_analysis/type_verification/verify.h"

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

VerificationTask TypeVerifier::VerifyType(ast::UnaryOperator const *node) {
  std::span operand_qts = co_await VerifyTypeOf(node->operand());

  QualifiedType qt;

  if (operand_qts.size() != 1) {
    ConsumeDiagnostic(UnexpandedUnaryOperatorArgument{
        .num_arguments = operand_qts.size(),
        .view          = node->operand()->range(),
    });

    qt = Error();
  } else if (operand_qts[0].qualifiers() >= Qualifiers::Error()) {
    qt = Error();
  } else {
    QualifiedType operand_qt      = operand_qts[0];
    core::Type operand_type       = operand_qt.type();
    Qualifiers operand_qualifiers = operand_qt.qualifiers();

    switch (node->kind()) {
      case ast::UnaryOperator::Kind::BufferPointer: {
        qt = QualifiedType(Type, operand_qualifiers & ~Qualifiers::Buffer());
        if (operand_type != Type) {
          ConsumeDiagnostic(NotAType{
              .view = node->operand()->range(),
              .type = TypeForDiagnostic(*node->operand()),
          });
          qt = Error(qt);
        }
      } break;
      case ast::UnaryOperator::Kind::TypeOf: {
        qt = Constant(Type);
      } break;
      case ast::UnaryOperator::Kind::At: {
        if (auto ptr_type =
                operand_type.get_if<core::PointerType>(type_system())) {
          qt = Reference(ptr_type->pointee());
        } else if (auto ptr_type =
                       operand_type.get_if<BufferPointerType>(type_system())) {
          qt = QualifiedType(ptr_type->pointee(), Qualifiers::Buffer());
        } else {
          ConsumeDiagnostic(DereferencingNonPointer{
              .type = TypeForDiagnostic(*node->operand()),
              .view = node->range(),
          });
          qt = Error();
        }
      } break;
      case ast::UnaryOperator::Kind::Address: {
        if (operand_qualifiers >= Qualifiers::Buffer()) {
          qt = QualifiedType(BufferPointerType(type_system(), operand_type));
        } else if (operand_qualifiers >= Qualifiers::Reference()) {
          qt = QualifiedType(core::PointerType(type_system(), operand_type));
        } else {
          ConsumeDiagnostic(NonAddressableExpression{.view = node->range()});
          qt = Error();
        }
      } break;
      case ast::UnaryOperator::Kind::Pointer: {
        qt = QualifiedType(Type, operand_qualifiers & ~Qualifiers::Buffer());
        if (operand_type != Type) {
          ConsumeDiagnostic(NotAType{
              .view = node->operand()->range(),
              .type = TypeForDiagnostic(*node->operand()),
          });
          qt = Error(qt);
        }
      } break;
      case ast::UnaryOperator::Kind::Negate: {
        if (operand_type == Integer or operand_type == F32 or
            operand_type == F64) {
          qt = QualifiedType(operand_type,
                             operand_qualifiers & Qualifiers::Constant());
        } else if (auto i = operand_type.get_if<core::SizedIntegerType>(
                       type_system())) {
          if (i->is_signed()) {
            qt = QualifiedType(operand_type,
                               operand_qualifiers & Qualifiers::Constant());

          } else {
            ConsumeDiagnostic(NegatingUnsignedInteger{
                .type = TypeForDiagnostic(*node->operand()),
                .view = node->range(),
            });
            qt = Error(operand_qualifiers & Qualifiers::Constant());
          }
        } else {
          ConsumeDiagnostic(InvalidUnaryOperatorCall{
              .op   = "-",
              .type = TypeForDiagnostic(*node->operand()),
              .view = node->range(),
          });
          qt = Error(operand_qualifiers & Qualifiers::Constant());
        }
      } break;
      case ast::UnaryOperator::Kind::Not: {
        if (operand_type == Bool) {
          qt = QualifiedType(Bool, operand_qualifiers & Qualifiers::Constant());
          // } else if (operand_type.is<type::Flags>()) {
          //   qt = type::QualType(
          //       operand_type, operand_qt.quals() &
          //       type::Qualifiers::Constant());
        } else {
          ConsumeDiagnostic(InvalidUnaryOperatorCall{
              .op   = "not",
              .type = TypeForDiagnostic(*node->operand()),
              .view = node->range(),
          });
          qt = Error(operand_qualifiers & Qualifiers::Constant());
        }
      } break;
      default: NTH_UNREACHABLE("{}") <<= {node->DebugString()};
    }
  }
  co_return TypeOf(node, qt);
}

}  // namespace semantic_analysis
