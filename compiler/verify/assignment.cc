#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/verify/common.h"
#include "compiler/verify/verify.h"
#include "type/cast.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct TypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "assignment-type-mismatch";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Cannot assign a value of type `%s` to a reference of type `%s`:",
            rhs_type, lhs_type),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string lhs_type;
  std::string rhs_type;
  std::string_view view;
};

struct AssigningToNonReference {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "assigning-to-non-reference";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Assigning to a non-reference expression:"),
        diagnostic::SourceQuote().Highlighted(lhs,
                                              diagnostic::Style::ErrorText()));
  }

  std::string_view lhs;
};

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::Assignment const *node) {
  std::vector<type::QualType> lhs_qts, rhs_qts;
  lhs_qts.reserve(node->lhs().size());
  rhs_qts.reserve(node->rhs().size());

  int first_lhs_error_index = -1;
  for (int i = 0; i < node->lhs().size(); ++i) {
    auto const *l = node->lhs()[i];
    LOG("Assignment", "lhs %u: %s", i, l->DebugString());

    auto qts = VerifyType(l);
    ASSERT(qts.size() == 1u);
    auto qt = qts[0];
    if (not qt.ok()) {
      if (first_lhs_error_index == -1) { first_lhs_error_index = i; }
    } else {
      if (qt.quals() >= type::Quals::Const()) {
        diag().Consume(AssigningToConstant{
            .to = TypeForDiagnostic(l, context()), .view = l->range()});
      } else if (not(qt.quals() >= type::Quals::Ref())) {
        diag().Consume(AssigningToNonReference{.lhs = l->range()});
      }
    }
    lhs_qts.push_back(qt);
  }

  int first_rhs_error_index = -1;
  for (int i = 0; i < node->rhs().size(); ++i) {
    auto const *r = node->rhs()[i];
    LOG("Assignment", "rhs %u: %s", i, r->DebugString());

    auto qts = VerifyType(r);
    if (not qts.empty() and not qts[0].ok()) {
      if (first_rhs_error_index == -1) {
        first_rhs_error_index = rhs_qts.size();
      }
    } else {
      rhs_qts.insert(rhs_qts.end(), qts.begin(), qts.end());
    }
  }

  auto lhs_iter = lhs_qts.begin();
  auto rhs_iter = rhs_qts.begin();

  auto lhs_end = (first_lhs_error_index == -1)
                     ? lhs_qts.end()
                     : lhs_qts.begin() + first_lhs_error_index;
  auto rhs_end = (first_rhs_error_index == -1)
                     ? rhs_qts.end()
                     : rhs_qts.begin() + first_rhs_error_index;

  while (true) {
    if (lhs_iter == lhs_end or rhs_iter == rhs_end) { break; }

    // TODO: deal with immovable and uncopyable types.
    type::Type lhs_type = (*lhs_iter).type();
    type::Type rhs_type = (*rhs_iter).type();
    if (not type::CanCastImplicitly(rhs_type, lhs_type)) {
      diag().Consume(TypeMismatch{
          .lhs_type = TypeForDiagnostic(
              node->lhs()[std::distance(lhs_qts.begin(), lhs_iter)], context()),
          .rhs_type = TypeForDiagnostic(
              node->rhs()[std::distance(rhs_qts.begin(), rhs_iter)], context()),
          // TODO: set the range to point more directly to the things we care
          // about.
          .view = node->range(),
      });
    }
    ++lhs_iter;
    ++rhs_iter;
  }

  return {};
}

}  // namespace compiler
