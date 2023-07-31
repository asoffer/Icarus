#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/common_diagnostics.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
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
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  size_t num_arguments;
  std::string_view view;
};

struct LogicalBinaryOperatorNeedsBool {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "logical-binary-operator-needs-bool";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Operator '%s' must be called with boolean arguments.",
                         ast::BinaryOperator::Symbol(kind)),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  ast::BinaryOperator::Kind kind;
  std::string_view view;
};

template <char Op>
core::Type VerifyArithmeticOperatorImpl(TypeVerifier &tv,
                                        ast::BinaryOperator const *node,
                                        core::Type lhs, core::Type rhs) {
  // TODO: This implementation is incomplete and incorrect.
  if (IsIntegral(lhs) and IsIntegral(rhs)) {
    if (lhs == rhs) { return lhs; }
    if (lhs == Integer) { return rhs; }
    if (rhs == Integer) { return lhs; }
    NTH_UNIMPLEMENTED();
  } else if (lhs == F64) {
    if (rhs == F64 or rhs == Integer) { return F64; }
    NTH_UNIMPLEMENTED();
  } else if (rhs == F64) {
    if (lhs == Integer) { return F64; }
    NTH_UNIMPLEMENTED();
  } else if (lhs == F32) {
    if (rhs == F32 or rhs == Integer) { return F32; }
    NTH_UNIMPLEMENTED();
  } else if (rhs == F32) {
    if (lhs == Integer) { return F32; }
    NTH_UNIMPLEMENTED();
  } else {
    NTH_UNIMPLEMENTED();
  }
}

template <char Op>
QualifiedType VerifyArithmeticOperator(TypeVerifier &tv,
                                       ast::BinaryOperator const *node,
                                       QualifiedType lhs, QualifiedType rhs) {
  auto t = VerifyArithmeticOperatorImpl<Op>(tv, node, lhs.type(), rhs.type());
  return QualifiedType(
      t, lhs.qualifiers() & rhs.qualifiers() & Qualifiers::Temporary());
}

}  // namespace

VerificationTask TypeVerifier::VerifyType(ast::BinaryOperator const *node) {
  std::span lhs_qts = co_await VerifyTypeOf(&node->lhs());
  std::span rhs_qts = co_await VerifyTypeOf(&node->rhs());

  bool can_continue = true;
  if (lhs_qts.size() != 1) {
    ConsumeDiagnostic(UnexpandedBinaryOperatorArgument{
        .num_arguments = lhs_qts.size(),
        .view          = node->lhs().range(),
    });
    can_continue = false;
  }

  if (rhs_qts.size() != 1) {
    ConsumeDiagnostic(UnexpandedBinaryOperatorArgument{
        .num_arguments = rhs_qts.size(),
        .view          = node->rhs().range(),
    });
    can_continue = false;
  }

  if (not can_continue) { co_return TypeOf(node, Error()); }

  auto lhs_qt = lhs_qts[0];
  auto rhs_qt = rhs_qts[0];

  QualifiedType qt;
  switch (node->kind()) {
    case ast::BinaryOperator::Kind::Add: {
      qt = VerifyArithmeticOperator<'+'>(*this, node, lhs_qt, rhs_qt);
    } break;
    case ast::BinaryOperator::Kind::Sub: {
      qt = VerifyArithmeticOperator<'-'>(*this, node, lhs_qt, rhs_qt);
    } break;
    case ast::BinaryOperator::Kind::Mul: {
      qt = VerifyArithmeticOperator<'*'>(*this, node, lhs_qt, rhs_qt);
    } break;
    case ast::BinaryOperator::Kind::Div: {
      qt = VerifyArithmeticOperator<'/'>(*this, node, lhs_qt, rhs_qt);
    } break;
    case ast::BinaryOperator::Kind::Mod: {
      qt = VerifyArithmeticOperator<'%'>(*this, node, lhs_qt, rhs_qt);
    } break;
    case ast::BinaryOperator::Kind::And:
    case ast::BinaryOperator::Kind::Or:
    case ast::BinaryOperator::Kind::Xor: {
      if (lhs_qt.type() == Bool and rhs_qt.type() == Bool) {
        qt = QualifiedType(Bool, lhs_qt.qualifiers() & rhs_qt.qualifiers() &
                                     Qualifiers::Temporary());
      } else {
        // `and`, `or`, and `xor` cannot be overloaded.
        ConsumeDiagnostic(LogicalBinaryOperatorNeedsBool{
            .kind = node->kind(),
            .view = node->operator_range(),
        });
        qt = Error();
      }
    } break;
    case ast::BinaryOperator::Kind::SymbolAnd:
    case ast::BinaryOperator::Kind::SymbolOr:
    case ast::BinaryOperator::Kind::SymbolXor:
    case ast::BinaryOperator::Kind::BlockJump: NTH_UNIMPLEMENTED();
  }

  co_return TypeOf(node, qt);
}

}  // namespace semantic_analysis
