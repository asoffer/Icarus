#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/common.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct UnexpandedUnaryOperatorArgument {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "unexpanded-unary-operator-argument";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Unary operator argument expands to %u values. Each "
                         "operand must expand to exactly 1 value.",
                         num_arguments),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  size_t num_arguments;
  frontend::SourceRange range;
};

struct UncopyableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncopyable-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to copy an uncopyable type `%s`.", from),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  type::Type from;
  frontend::SourceRange range;
};

struct NonConstantInterface {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-interface";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Non-constant type in interface constructor `~`"),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
};

struct InvalidUnaryOperatorOverload {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "invalid-unary-operator-overload";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No valid operator overload for (%s)", op),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
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
            type),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  char const *op;
  std::string type;
  frontend::SourceRange range;
};

struct NegatingUnsignedInteger {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "negating-unsigned-integer";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to negate an unsigned integer of type `%s`.", type),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  type::Type type;
  frontend::SourceRange range;
};

struct NonAddressableExpression {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "non-addressable-expression";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expression is not addressable."),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
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
                         type),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  type::Type type;
  frontend::SourceRange range;
};

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(ast::UnaryOperator const *node) {
  auto operand_qts = VerifyType(node->operand());

  if (operand_qts.size() != 1) {
    diag().Consume(UnexpandedUnaryOperatorArgument{
        .num_arguments = operand_qts.size(),
        .range         = node->operand()->range(),
    });
    return context().set_qual_type(node, type::QualType::Error());
  }

  if (not operand_qts[0].ok()) {
    return context().set_qual_type(node, type::QualType::Error());
  }
  auto operand_qt   = operand_qts[0];
  auto operand_type = operand_qt.type();

  type::QualType qt;

  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Copy: {
      ASSERT(operand_type.get()->completeness() ==
             type::Completeness::Complete);
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
    case ast::UnaryOperator::Kind::Destroy: {
      qt = type::QualType::NonConstant(type::Void);
    } break;
    case ast::UnaryOperator::Kind::Move: {
      ASSERT(operand_type.get()->completeness() ==
             type::Completeness::Complete);
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
        qt = type::QualType::Error();
      }
    } break;
    case ast::UnaryOperator::Kind::TypeOf: {
      qt = type::QualType::Constant(type::Type_);
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
        qt = type::QualType::Error();
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
        qt = type::QualType::Error();
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
        qt = type::QualType::Error();
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
        qt = type::QualType::Error();
      } else if (operand_type.is<type::Struct>()) {
        // TODO: support calling with constant arguments.
        qt = VerifyUnaryOverload(
            "-", node, type::Typed<ir::Value>(ir::Value(), operand_qt.type()));
        if (not qt.ok()) {
          diag().Consume(InvalidUnaryOperatorOverload{
              .op    = "-",
              .range = node->range(),
          });
        }
      } else {
        diag().Consume(InvalidUnaryOperatorCall{
            .op    = "-",
            .type  = TypeForDiagnostic(node->operand(), context()),
            .range = node->range(),
        });
        qt = type::QualType::Error();
      }
    } break;
    case ast::UnaryOperator::Kind::Not: {
      if (operand_type == type::Bool) {
        qt = type::QualType(type::Bool,
                            operand_qt.quals() & type::Quals::Const());
      } else if (operand_type.is<type::Flags>()) {
        qt = type::QualType(operand_type,
                            operand_qt.quals() & type::Quals::Const());
      } else if (operand_type.is<type::Struct>()) {
        // TODO: support calling with constant arguments.
        qt = VerifyUnaryOverload(
            "not", node,
            type::Typed<ir::Value>(ir::Value(), operand_qt.type()));
        if (not qt.ok()) {
          diag().Consume(InvalidUnaryOperatorOverload{
              .op    = "not",
              .range = node->range(),
          });
          qt = type::QualType::Error();
        }
      } else {
        diag().Consume(InvalidUnaryOperatorCall{
            .op    = "not",
            .type  = TypeForDiagnostic(node->operand(), context()),
            .range = node->range(),
        });
        qt = type::QualType::Error();
      }
    } break;
    default: UNREACHABLE(*node);
  }

  return context().set_qual_type(node, qt);
}

void Compiler::VerifyPatternType(ast::UnaryOperator const *node, type::Type t) {
  context().set_qual_type(node, type::QualType::Constant(t));
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Not:
      if (t != type::Bool) {
        diag().Consume(PatternTypeMismatch{
            .pattern_type = t,
            .matched_type = "bool",
            .range        = node->range(),
        });
      }
      VerifyPatternType(node->operand(), type::Bool);
      break;
    case ast::UnaryOperator::Kind::BufferPointer:
    case ast::UnaryOperator::Kind::Pointer:
      if (t != type::Type_) {
        diag().Consume(PatternTypeMismatch{
            .pattern_type = t,
            .matched_type = "type",
            .range        = node->range(),
        });
      }
      VerifyPatternType(node->operand(), type::Type_);
      break;
    case ast::UnaryOperator::Kind::Negate:
      if (not type::IsNumeric(t)) {
        diag().Consume(PatternTypeMismatch{
            .pattern_type = t,
            .matched_type = "Must be a numeric primitive type",
            .range        = node->range(),
        });
      } else {
        VerifyPatternType(node->operand(), t);
      }
      break;
    case ast::UnaryOperator::Kind::Copy:
    case ast::UnaryOperator::Kind::Init:
    case ast::UnaryOperator::Kind::Move: {
      VerifyPatternType(node->operand(), t);
    } break;
    default: UNREACHABLE(node->DebugString());
  }
}

}  // namespace compiler
