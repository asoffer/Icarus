#include <functional>

#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/module.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/common.h"
#include "compiler/verify/verify.h"
#include "type/cast.h"
#include "type/flags.h"
#include "type/overload_set.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/provenance.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct UnexpandedBinaryOperatorArgument {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "unexpanded-binary-operator-argument";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Binary operator argument expands to %u values. Each "
                         "operand must expand to exactly 1 value.",
                         num_arguments),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  size_t num_arguments;
  frontend::SourceView view;
};

struct LogicalBinaryOperatorNeedsBool {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "logical-binary-operator-needs-bool";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Operator '%s' must be called with boolean arguments.",
                         ast::BinaryOperator::Symbol(kind)),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  ast::BinaryOperator::Kind kind;
  frontend::SourceView view;
};

struct InvalidAssignmentOperatorLhsValueCategory {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName =
      "invalid-assignment-lhs-value-category";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Lefthand-side of binary logical assignment operator "
                         "must not be constant."),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  frontend::SourceView view;
};

struct BinaryOperatorTypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "binary-operator-type-mismatch";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Mismatched types `%s` and `%s` in binary operator.",
                         lhs_type, rhs_type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  type::Type lhs_type;
  type::Type rhs_type;
  frontend::SourceView view;
};

struct NoMatchingBinaryOperator {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-matching-binary-operator";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "No matching binary operator (%s) for types `%s` and `%s`.", op,
            lhs_type, rhs_type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  std::string op;
  std::string lhs_type;
  std::string rhs_type;
  frontend::SourceView view;
};

template <
    base::one_of<ast::BinaryOperator, ast::BinaryAssignmentOperator> NodeType>
type::QualType VerifyOperatorOverload(
    TypeVerifier &tv, NodeType const *node,
    type::Typed<ir::CompleteResultRef> const &lhs,
    type::Typed<ir::CompleteResultRef> const &rhs) {
  auto symbol = NodeType::Symbol(node->kind());
  CallMetadata metadata(symbol, node->scope(),
                        ModulesFromTypeProvenance({lhs.type(), rhs.type()}));
  if (metadata.overloads().empty()) { return type::QualType::Error(); }

  absl::flat_hash_set<type::Function const *> member_types;
  CallMetadata::callee_locator_t resolved_call =
      static_cast<ast::Expression const *>(nullptr);
  for (auto overload : metadata.overloads()) {
    type::QualType qt;
    if (auto const *e = overload.get_if<ast::Expression>()) {
      if (auto qts =
              ModuleFor(e)->as<CompiledModule>().context().maybe_qual_type(e);
          not qts.empty()) {
        ASSIGN_OR(continue, qt, qts[0]);
      }
    } else {
      qt = overload.get<module::Module::SymbolInformation>()->qualified_type;
    }

    // Must be callable because we're looking at overloads for operators
    // which have previously been type-checked to ensure callability.
    auto &c = qt.type().as<type::Function>();
    member_types.insert(&c);
    resolved_call = overload;
  }

  tv.context().SetCallMetadata(node, CallMetadata(resolved_call));

  ASSERT(member_types.size() == 1u);
  return type::QualType((*member_types.begin())->return_types()[0],
                        type::Quals::Unqualified());
}

template <char C>
absl::Span<type::QualType const> VerifyArithmeticOperator(
    TypeVerifier &tv, ast::BinaryOperator const *node, type::QualType lhs_qt,
    type::QualType rhs_qt) {
  if (type::IsNumeric(lhs_qt.type()) and type::IsNumeric(rhs_qt.type())) {
    if (auto t = type::Meet(rhs_qt.type(), lhs_qt.type())) {
      auto quals = (lhs_qt.quals() & rhs_qt.quals() & ~type::Quals::Ref());
      return tv.context().set_qual_type(node, type::QualType(t, quals));
    } else {
      tv.diag().Consume(BinaryOperatorTypeMismatch{
          .lhs_type = lhs_qt.type(),
          .rhs_type = rhs_qt.type(),
          .view     = SourceViewFor(node),
      });
      return tv.context().set_qual_type(node, type::QualType::Error());
    }
  }

  if constexpr (C == '+') {
    if (lhs_qt.type().is<type::BufferPointer>() and
        type::IsIntegral(rhs_qt.type())) {
      return tv.context().set_qual_type(node, lhs_qt);
    } else if (rhs_qt.type().is<type::BufferPointer>() and
               type::IsIntegral(lhs_qt.type())) {
      return tv.context().set_qual_type(node, rhs_qt);
    }
  }

  if constexpr (C == '-') {
    if (lhs_qt.type().is<type::BufferPointer>()) {
      if (type::IsIntegral(rhs_qt.type())) {
        return tv.context().set_qual_type(node, lhs_qt);
      } else if (lhs_qt.type() == rhs_qt.type()) {
        auto quals = (lhs_qt.quals() & rhs_qt.quals() & ~type::Quals::Ref());
        return tv.context().set_qual_type(
            node, type::QualType(
                      type::PointerDifferenceType(tv.resources().architecture),
                      quals));
      }
    }
  }

  // TODO: Calling with constants?
  auto qt = VerifyOperatorOverload<ast::BinaryOperator>(
      tv, node,
      type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                         lhs_qt.type()),
      type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                         rhs_qt.type()));
  if (not qt.ok()) {
    tv.diag().Consume(NoMatchingBinaryOperator{
        .op       = std::string(ast::BinaryOperator::Symbol(node->kind())),
        .lhs_type = TypeForDiagnostic(&node->lhs(), tv.context()),
        .rhs_type = TypeForDiagnostic(&node->rhs(), tv.context()),
        .view     = SourceViewFor(node),
    });
  }
  return tv.context().set_qual_type(node, qt);
}

std::optional<std::pair<type::QualType, type::QualType>> VerifyOperands(
    TypeVerifier &tv, ast::BinaryOperator const *node) {
  auto lhs_qts = VerifyType(tv, &node->lhs());
  auto rhs_qts = VerifyType(tv, &node->rhs());

  bool error = false;
  if (lhs_qts.size() != 1) {
    tv.diag().Consume(UnexpandedBinaryOperatorArgument{
        .num_arguments = lhs_qts.size(),
        .view          = SourceViewFor(&node->lhs()),
    });
    error = true;
  }

  if (rhs_qts.size() != 1) {
    tv.diag().Consume(UnexpandedBinaryOperatorArgument{
        .num_arguments = rhs_qts.size(),
        .view          = SourceViewFor(&node->rhs()),
    });
    error = true;
  }

  if (error or not lhs_qts[0].ok() or not rhs_qts[0].ok()) {
    return std::nullopt;
  }

  return std::make_pair(lhs_qts[0], rhs_qts[0]);
}

template <char C>
absl::Span<type::QualType const> VerifyArithmeticAssignment(
    TypeVerifier &tv, ast::BinaryAssignmentOperator const *node,
    type::QualType lhs_qt, type::QualType rhs_qt) {
  if (type::IsNumeric(lhs_qt.type()) and type::IsNumeric(rhs_qt.type())) {
    if (type::CanCastImplicitly(rhs_qt.type(), lhs_qt.type())) {
      return tv.context().set_qual_types(node, {});
    } else {
      tv.diag().Consume(BinaryOperatorTypeMismatch{
          .lhs_type = lhs_qt.type(),
          .rhs_type = rhs_qt.type(),
          .view     = SourceViewFor(node),
      });
      return tv.context().set_qual_type(node, type::QualType::Error());
    }
  }

  if constexpr (C == '+') {
    if (lhs_qt.type().is<type::BufferPointer>() and
        type::IsIntegral(rhs_qt.type())) {
      return tv.context().set_qual_types(node, {});
    }
  }

  if constexpr (C == '-') {
    if (lhs_qt.type().is<type::BufferPointer>() and
        type::IsIntegral(rhs_qt.type())) {
      return tv.context().set_qual_types(node, {});
    }
  }

  // TODO: Calling with constants?
  auto qt = VerifyOperatorOverload<ast::BinaryAssignmentOperator>(
      tv, node,
      type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                         lhs_qt.type()),
      type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                         rhs_qt.type()));
  if (not qt.ok()) {
    tv.diag().Consume(NoMatchingBinaryOperator{
        .op = std::string(ast::BinaryAssignmentOperator::Symbol(node->kind())),
        .lhs_type = TypeForDiagnostic(&node->lhs(), tv.context()),
        .rhs_type = TypeForDiagnostic(&node->rhs(), tv.context()),
        .view     = SourceViewFor(node),
    });
  }

  return tv.context().set_qual_type(node, type::QualType::Error());
}

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::BinaryOperator const *node) {
  auto result = VerifyOperands(*this, node);
  if (not result) {
    return context().set_qual_type(node, type::QualType::Error());
  }
  auto [lhs_qt, rhs_qt] = *result;

  switch (node->kind()) {
    case ast::BinaryOperator::Kind::Xor:
    case ast::BinaryOperator::Kind::And:
    case ast::BinaryOperator::Kind::Or: {
      if (lhs_qt.type() == type::Bool and rhs_qt.type() == type::Bool) {
        auto quals = (lhs_qt.quals() & rhs_qt.quals() & ~type::Quals::Ref());
        type::QualType qt(type::Meet(lhs_qt.type(), rhs_qt.type()), quals);
        return context().set_qual_type(node, qt);
      } else {
        // `and`, `or`, and `xor` cannot be overloaded.

        // TODO: Get an actual range for the operator.
        diag().Consume(LogicalBinaryOperatorNeedsBool{
            .kind = node->kind(),
            .view = SourceViewFor(node),
        });
      }
      auto qt = type::QualType::NonConstant(type::Bool);
      qt.MarkError();
      return context().set_qual_type(node, qt);
    }
    case ast::BinaryOperator::Kind::SymbolXor:
    case ast::BinaryOperator::Kind::SymbolAnd:
    case ast::BinaryOperator::Kind::SymbolOr: {
      if (lhs_qt.type().is<type::Flags>() and rhs_qt.type().is<type::Flags>()) {
        if (auto t = type::Meet(rhs_qt.type(), lhs_qt.type())) {
          auto quals = (lhs_qt.quals() & rhs_qt.quals() & ~type::Quals::Ref());
          return context().set_qual_type(node, type::QualType(t, quals));
        } else {
          diag().Consume(BinaryOperatorTypeMismatch{
              .lhs_type = lhs_qt.type(),
              .rhs_type = rhs_qt.type(),
              .view     = SourceViewFor(node),
          });
          return context().set_qual_type(node, type::QualType::Error());
        }
      } else {
        auto qt = VerifyOperatorOverload<ast::BinaryOperator>(
            *this, node,
            type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                               lhs_qt.type()),
            type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                               rhs_qt.type()));
        if (not qt.ok()) {
          diag().Consume(NoMatchingBinaryOperator{
              .op = std::string(ast::BinaryOperator::Symbol(node->kind())),
              .lhs_type = TypeForDiagnostic(&node->lhs(), context()),
              .rhs_type = TypeForDiagnostic(&node->rhs(), context()),
              .view     = SourceViewFor(node),
          });
        }
        return context().set_qual_type(node, qt);
      }
    }
    case ast::BinaryOperator::Kind::Add:
      return VerifyArithmeticOperator<'+'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Sub:
      return VerifyArithmeticOperator<'-'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Mul:
      return VerifyArithmeticOperator<'*'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Div:
      return VerifyArithmeticOperator<'/'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Mod:
      return VerifyArithmeticOperator<'%'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::BlockJump: {
      // TODO: Look at the scope context and determine who might jump to here
      // and what they might attempt to return.
      return context().set_qual_type(node,
                                     type::QualType::Constant(type::Void));

      // std::optional<ir::CompleteResultBuffer> constant_buffer;
      // core::Arguments<type::Typed<ir::CompleteResultRef>> arguments;
      // if (lhs_qt.constant()) {
      //   constant_buffer = EvaluateToBufferOrDiagnose(
      //       type::Typed(&node->lhs(), lhs_qt.type()));
      // }

      // if (constant_buffer) {
      //   arguments.pos_emplace((*constant_buffer)[0], lhs_qt.type());
      // } else {
      //   arguments.pos_emplace(ir::CompleteResultRef(), lhs_qt.type());
      // }

      // auto qts_or_errors = VerifyReturningCall(
      //     *this, {.callee = &node->rhs(), .arguments =
      //     std::move(arguments)});
      // if (auto *errors =
      //         std::get_if<absl::flat_hash_map<type::Callable const *,
      //                                         core::CallabilityResult>>(
      //             &qts_or_errors)) {
      //   // TODO Argument span??
      //   diag().Consume(
      //       UncallableError(context(), &node->lhs(), {},
      //       std::move(*errors)));
      //   return context().set_qual_type(node, type::QualType::Error());
      // }

      // return context().set_qual_types(
      //     node, std::get<std::vector<type::QualType>>(qts_or_errors));
    }
  }
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::BinaryAssignmentOperator const *node) {
  auto result = VerifyOperands(*this, node);
  if (not result) {
    return context().set_qual_type(node, type::QualType::Error());
  }
  auto [lhs_qt, rhs_qt] = *result;

  if (lhs_qt.quals() >= type::Quals::Const() or
      not(lhs_qt.quals() >= type::Quals::Ref())) {
    diag().Consume(InvalidAssignmentOperatorLhsValueCategory{
        .view = SourceViewFor(&node->lhs()),
    });
  }

  switch (node->kind()) {
    case ast::BinaryOperator::Kind::SymbolXor:
    case ast::BinaryOperator::Kind::SymbolAnd:
    case ast::BinaryOperator::Kind::SymbolOr: {
      if (lhs_qt.type() == rhs_qt.type() and lhs_qt.type().is<type::Flags>()) {
        return context().set_qual_type(node, lhs_qt);
      } else {
        auto qt = VerifyOperatorOverload<ast::BinaryOperator>(
            *this, node,
            type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                               lhs_qt.type()),
            type::Typed<ir::CompleteResultRef>(ir::CompleteResultRef(),
                                               rhs_qt.type()));
        if (not qt.ok()) {
          diag().Consume(NoMatchingBinaryOperator{
              .op = std::string(ast::BinaryOperator::Symbol(node->kind())),
              .lhs_type = TypeForDiagnostic(&node->lhs(), context()),
              .rhs_type = TypeForDiagnostic(&node->rhs(), context()),
              .view     = SourceViewFor(node),
          });
        }
        return context().set_qual_type(node, qt);
      }
    } break;
    case ast::BinaryOperator::Kind::Add:
      return VerifyArithmeticAssignment<'+'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Sub:
      return VerifyArithmeticAssignment<'-'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Mul:
      return VerifyArithmeticAssignment<'*'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Div:
      return VerifyArithmeticAssignment<'/'>(*this, node, lhs_qt, rhs_qt);
    case ast::BinaryOperator::Kind::Mod:
      return VerifyArithmeticAssignment<'%'>(*this, node, lhs_qt, rhs_qt);
    default: UNREACHABLE(node->kind()); break;
  }
}

bool PatternTypeVerifier::VerifyPatternType(ast::BinaryOperator const *node,
                                            type::Type t) {
  context().set_qual_type(node, type::QualType::Constant(t));
  switch (node->kind()) {
    case ast::BinaryOperator::Kind::Add:
    case ast::BinaryOperator::Kind::Sub:
    case ast::BinaryOperator::Kind::Mul: {
      // TODO: Support non-builtin types.
      if (node->rhs().covers_binding()) {
        if (node->lhs().covers_binding()) {
          NOT_YET();
        } else {
          VerifyType(*this, &node->lhs());
          state().EnqueueVerifyPatternMatchType(&node->rhs(), t);
        }
      }

      if (node->lhs().covers_binding()) {
        if (node->lhs().covers_binding()) {
          VerifyType(*this, &node->rhs());
          state().EnqueueVerifyPatternMatchType(&node->lhs(), t);
        } else {
          NOT_YET();
        }
      }
    } break;

    default: NOT_YET(node->DebugString());
  }
  return true;
}

}  // namespace compiler
