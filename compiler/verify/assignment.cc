#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/verify/common.h"
#include "compiler/verify/internal/qual_type_iterator.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct TypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "assignment-type-mismatch";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Cannot assign a value of type `%s` to a reference of type `%s`:",
            lhs_type.to_string(), rhs_type.to_string()),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  type::Type lhs_type;
  type::Type rhs_type;
  frontend::SourceRange range;
};

struct AssigningToNonReference {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "assigning-to-non-reference";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Assigning to a non-reference expression:"),
        diagnostic::SourceQuote(src).Highlighted(
            lhs, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange lhs;
};

}  // namespace

type::QualType Compiler::VerifyType(ast::Assignment const *node) {
  std::vector<type::QualType> lhs_qts, rhs_qts;
  lhs_qts.reserve(node->lhs().size());
  rhs_qts.reserve(node->rhs().size());

  int first_lhs_error_index = -1;
  for (int i = 0; i < node->lhs().size(); ++i) {
    auto const *l = node->lhs()[i];

    auto qt = VerifyType(l);
    if (not qt.ok()) {
      if (first_lhs_error_index == -1) { first_lhs_error_index = i; }
    } else {
      if (qt.quals() >= type::Quals::Const()) {
        diag().Consume(
            AssigningToConstant{.to = qt.type(), .range = l->range()});
      } else if (not(qt.quals() >= type::Quals::Ref())) {
        diag().Consume(AssigningToNonReference{.lhs = l->range()});
      }
    }
    lhs_qts.push_back(qt);
  }

  int first_rhs_error_index = -1;
  for (int i = 0; i < node->lhs().size(); ++i) {
    auto const *r = node->rhs()[i];
    auto qt       = VerifyType(r);
    if (not qt.ok()) {
      if (first_lhs_error_index == -1) { first_lhs_error_index = i; }
    }
    rhs_qts.push_back(qt);
  }

  internal::QualTypeIterator lhs_iter(lhs_qts.begin());
  internal::QualTypeIterator rhs_iter(rhs_qts.begin());

  internal::QualTypeIterator const lhs_end(
      (first_lhs_error_index == -1) ? lhs_qts.end()
                                    : lhs_qts.begin() + first_lhs_error_index);
  internal::QualTypeIterator const rhs_end(
      (first_rhs_error_index == -1) ? rhs_qts.end()
                                    : rhs_qts.begin() + first_rhs_error_index);

  while (true) {
    if (lhs_iter == lhs_end or rhs_iter == rhs_end) { break; }

    // TODO: deal with immovable and uncopyable types.
    type::Type lhs_type = (*lhs_iter).type();
    type::Type rhs_type = (*rhs_iter).type();
    if (lhs_type != rhs_type) {
      diag().Consume(TypeMismatch{
          .lhs_type = lhs_type,
          .rhs_type = rhs_type,
          // TODO: set the range to point more directly to the things we care
          // about.
          .range = node->range(),
      });
    }
    ++lhs_iter;
    ++rhs_iter;
  }

  return type::QualType::Constant(type::Void());
}

}  // namespace compiler
