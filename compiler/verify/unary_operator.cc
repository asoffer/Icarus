#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/common.h"
#include "type/flags.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/struct.h"

namespace compiler {
namespace {

struct UnexpandedUnaryOperatorArgument {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "unexpanded-unary-operator-argument";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Unary operator argument expands to %u values. Each "
                         "operand must expand to exactly 1 value.",
                         num_arguments),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  size_t num_arguments;
  frontend::SourceView view;
};

struct UncopyableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncopyable-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to copy an uncopyable type `%s`.", from),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  type::Type from;
  frontend::SourceView view;
};

struct NonConstantInterface {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-interface";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Non-constant type in interface constructor `~`"),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  frontend::SourceView view;
};

struct InvalidUnaryOperatorOverload {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "invalid-unary-operator-overload";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No valid operator overload for (%s)", op),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  char const *op;
  frontend::SourceView view;
};

struct InvalidUnaryOperatorCall {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-unary-operator-call";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Invalid call to unary operator (%s) with argument type `%s`", op,
            type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  char const *op;
  std::string type;
  frontend::SourceView view;
};

struct NegatingUnsignedInteger {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "negating-unsigned-integer";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to negate an unsigned integer of type `%s`.", type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  type::Type type;
  frontend::SourceView view;
};

struct NonAddressableExpression {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "non-addressable-expression";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expression is not addressable."),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  frontend::SourceView view;
};

struct DereferencingNonPointer {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "dereferencing-non-pointer";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to dereference an object of type `%s` "
                         "which is not a pointer",
                         type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  type::Type type;
  frontend::SourceView view;
};

// TODO: Replace `symbol` with an enum.
type::QualType VerifyUnaryOverload(
    Context &context, char const *symbol, ast::Expression const *node,
    type::Typed<ir::CompleteResultRef> const &operand) {
  absl::flat_hash_set<type::Function const *> member_types;

  ForEachSymbolQualType(node->scope(), symbol, [&](type::QualType qt) {
    // Must be callable because we're looking at overloads for operators
    // which have previously been type-checked to ensure callability.
    auto &c = qt.type().as<type::Function>();
    member_types.insert(&c);
    return true;
  });

  if (member_types.empty()) {
    return context.set_qual_type(node, type::QualType::Error())[0];
  }

  ASSERT(member_types.size() == 1u);
  // TODO: Check that we only have one return type on each of these overloads.
  return type::QualType((*member_types.begin())->return_types()[0],
                        type::Quals::Unqualified());
}

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::UnaryOperator const *node) {
  auto operand_qts = VerifyType(node->operand());

  if (operand_qts.size() != 1) {
    diag().Consume(UnexpandedUnaryOperatorArgument{
        .num_arguments = operand_qts.size(),
        .view          = SourceViewFor(node->operand()),
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
      if (auto const *s = operand_type.if_as<type::Struct>()) {
        EnsureComplete({.kind    = WorkItem::Kind::CompleteStruct,
                        .node    = context().AstLiteral(s),
                        .context = &context()});
      }
      ASSERT(operand_type.get()->completeness() ==
             type::Completeness::Complete);
      if (not operand_type.get()->IsCopyable()) {
        diag().Consume(UncopyableType{
            .from = operand_type,
            .view = SourceViewFor(node),
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
      if (auto const *s = operand_type.if_as<type::Struct>()) {
        EnsureComplete({.kind    = WorkItem::Kind::CompleteStruct,
                        .node    = context().AstLiteral(s),
                        .context = &context()});
      }
      ASSERT(operand_type.get()->completeness() ==
             type::Completeness::Complete);
      if (not operand_type.get()->IsMovable()) {
        diag().Consume(ImmovableType{
            .from = operand_type,
            .view = SourceViewFor(node),
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
            .view = SourceViewFor(node->operand()),
            .type = operand_type,
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
            .type = operand_type,
            .view = SourceViewFor(node),
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
        diag().Consume(NonAddressableExpression{.view = SourceViewFor(node)});
        qt = type::QualType::Error();
      }
    } break;
    case ast::UnaryOperator::Kind::Pointer: {
      if (operand_type == type::Type_) {
        qt = type::QualType(operand_type,
                            operand_qt.quals() & ~type::Quals::Buf());
      } else {
        diag().Consume(NotAType{
            .view = SourceViewFor(node->operand()),
            .type = operand_type,
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
            .type = operand_type,
            .view = SourceViewFor(node),
        });
        qt = type::QualType::Error();
      } else if (operand_type.is<type::Struct>()) {
        // TODO: support calling with constant arguments.
        qt = VerifyUnaryOverload(
            context(), "-", node,
            type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                               operand_qt.type()));
        if (not qt.ok()) {
          diag().Consume(InvalidUnaryOperatorOverload{
              .op   = "-",
              .view = SourceViewFor(node),
          });
        }
      } else {
        diag().Consume(InvalidUnaryOperatorCall{
            .op   = "-",
            .type = TypeForDiagnostic(node->operand(), context()),
            .view = SourceViewFor(node),
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
            context(), "not", node,
            type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                               operand_qt.type()));
        if (not qt.ok()) {
          diag().Consume(InvalidUnaryOperatorOverload{
              .op   = "not",
              .view = SourceViewFor(node),
          });
          qt = type::QualType::Error();
        }
      } else {
        diag().Consume(InvalidUnaryOperatorCall{
            .op   = "not",
            .type = TypeForDiagnostic(node->operand(), context()),
            .view = SourceViewFor(node),
        });
        qt = type::QualType::Error();
      }
    } break;
    case ast::UnaryOperator::Kind::BlockJump: {
      // TODO: Look at the scope context and determine who might jump to here
      // and what they might attempt to return.
      qt = type::QualType::Constant(type::Void);

      // auto qts_or_errors =
      //     VerifyReturningCall(*this, {.callee = node->operand()});
      // if (auto *errors =
      //         std::get_if<absl::flat_hash_map<type::Callable const *,
      //                                         core::CallabilityResult>>(
      //             &qts_or_errors)) {
      //   diag().Consume(UncallableError(context(), node->operand(), {},
      //                                  std::move(*errors)));
      //   return context().set_qual_type(node, type::QualType::Error());
      // }
      // auto &qual_type = std::get<std::vector<type::QualType>>(qts_or_errors);
      // return context().set_qual_types(node, qual_type);
    } break;
    default: UNREACHABLE(node->DebugString());
  }

  return context().set_qual_type(node, qt);
}

bool PatternTypeVerifier::VerifyPatternType(ast::UnaryOperator const *node,
                                            type::Type t) {
  context().set_qual_type(node, type::QualType::Constant(t));
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::Not:
      if (t != type::Bool) {
        diag().Consume(PatternTypeMismatch{
            .pattern_type = t,
            .matched_type = "bool",
            .view         = SourceViewFor(node),
        });
        return false;
      }
      return VerifyPatternType(node->operand(), type::Bool);
      break;
    case ast::UnaryOperator::Kind::BufferPointer:
    case ast::UnaryOperator::Kind::Pointer:
      if (t != type::Type_) {
        diag().Consume(PatternTypeMismatch{
            .pattern_type = t,
            .matched_type = "type",
            .view         = SourceViewFor(node),
        });
        return false;
      }
      return VerifyPatternType(node->operand(), type::Type_);
      break;
    case ast::UnaryOperator::Kind::Negate:
      if (not type::IsNumeric(t)) {
        diag().Consume(PatternTypeMismatch{
            .pattern_type = t,
            .matched_type = "Must be a numeric primitive type",
            .view         = SourceViewFor(node),
        });
        return false;
      } else {
        return VerifyPatternType(node->operand(), t);
      }
      break;
    case ast::UnaryOperator::Kind::Copy:
    case ast::UnaryOperator::Kind::Init:
    case ast::UnaryOperator::Kind::Move: {
      return VerifyPatternType(node->operand(), t);
    } break;
    default: UNREACHABLE(node->DebugString());
  }
  return true;
}

}  // namespace compiler
