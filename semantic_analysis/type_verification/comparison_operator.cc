#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct ComparingIncomparables {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "comparing-incomparables";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Values of type `%s` and `%s` are being compared but no such "
            "comparison is allowed:",
            lhs, rhs),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string lhs;
  std::string rhs;
  std::string_view view;
};

enum class Relation { Unordered, Comparable, Ordered };

Relation Comparison(core::Type lhs, core::Type rhs, TypeSystem &ts) {
  // TODO: This implementation is incomplete and incorrect.
  if (IsIntegral(lhs)) {
    if (IsIntegral(rhs)) { return Relation::Ordered; }
    if (rhs.is<PrimitiveType>(ts)) { return Relation::Unordered; }
  } else if (auto lp = lhs.get_if<BufferPointerType>(ts)) {
    if (lhs == rhs) { return Relation::Ordered; }
    if (auto rp = rhs.get_if<core::PointerType>(ts)) {
      return lp->pointee() == rp->pointee() ? Relation::Comparable
                                            : Relation::Unordered;
    } else {
      return Relation::Unordered;
    }
  } else if (auto lp = lhs.get_if<core::PointerType>(ts)) {
    if (auto rp = rhs.get_if<BufferPointerType>(ts)) {
      return lp->pointee() == rp->pointee() ? Relation::Comparable
                                            : Relation::Unordered;
    } else if (auto rp = rhs.get_if<core::PointerType>(ts)) {
      return lp->pointee() == rp->pointee() ? Relation::Comparable
                                            : Relation::Unordered;
    } else {
      return Relation::Unordered;
    }
  } else if (lhs.is<PrimitiveType>(ts)) {
    if (lhs == Bool or lhs == Char or lhs == Byte or lhs == Type or
        lhs == Module) {
      return lhs == rhs ? Relation::Comparable : Relation::Unordered;
    } else if (lhs== F32 or lhs == F64) {
      // TODO: Support comparisons to integral types that can be done cheaply.
      return (rhs == F32 or rhs == F64) ? Relation::Ordered
                                        : Relation::Unordered;
    }
  } else if (auto le = lhs.get_if<EnumType>(ts)) {
    return le == rhs ? Relation::Comparable : Relation::Unordered;
  }

  NOT_YET(DebugType(lhs, ts), DebugType(rhs, ts));
}

}  // namespace

VerificationTask TypeVerifier::VerifyType(ast::ComparisonOperator const *node) {
  std::vector<core::Type> operand_types;
  Qualifiers qualifiers = Qualifiers::Constant();
  operand_types.reserve(node->exprs().size());
  bool all_length_one = true;
  for (auto const *expr : node->exprs()) {
    std::span expr_qts = co_await VerifyTypeOf(expr);
    if (expr_qts.size() == 1) {
      operand_types.push_back(expr_qts.front().type());
      qualifiers &= expr_qts.front().qualifiers();
    } else {
      all_length_one = false;
    }
  }
  if (not all_length_one) { co_return TypeOf(node, Error(Bool)); }

  QualifiedType qt(Bool, qualifiers);
  for (size_t i = 0; i < node->ops().size(); ++i) {
    auto const *lhs = node->exprs()[i];
    auto const *rhs = node->exprs()[i + 1];
    auto lhs_type   = operand_types[i];
    auto rhs_type   = operand_types[i + 1];
    auto op         = node->ops()[i];

    switch (Comparison(lhs_type, rhs_type, type_system())) {
      case Relation::Unordered: {
        ConsumeDiagnostic(ComparingIncomparables{
            .lhs  = TypeForDiagnostic(*lhs),
            .rhs  = TypeForDiagnostic(*rhs),
            .view = node->binary_range(i),
        });
        qt = Error(Bool);
      } break;
      case Relation::Comparable:
        if (op != frontend::Operator::Eq and op != frontend::Operator::Ne) {
          ConsumeDiagnostic(ComparingIncomparables{
              .lhs  = TypeForDiagnostic(*lhs),
              .rhs  = TypeForDiagnostic(*rhs),
              .view = node->binary_range(i),
          });
          qt = Error(Bool);
        }
      case Relation::Ordered: break;
    }
  }
  co_return TypeOf(node, qt);
}

}  // namespace semantic_analysis
