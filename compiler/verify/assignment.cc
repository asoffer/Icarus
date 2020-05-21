#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "base/defer.h"
#include "compiler/compiler.h"
#include "compiler/verify/internal/qual_type_iterator.h"
#include "diagnostic/errors.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {

type::QualType Compiler::VerifyType(ast::Assignment const *node) {
  std::vector<type::QualType> lhs_qts, rhs_qts;
  lhs_qts.reserve(node->lhs().size());
  rhs_qts.reserve(node->rhs().size());

  int first_lhs_error_index = -1;
  for (int i = 0; i < node->lhs().size(); ++i) {
    auto const *l = node->lhs()[i];
    // TODO check can't be constant. must be references
    auto qt = VerifyType(l);
    if (not qt.ok()) {
      if (first_lhs_error_index == -1) { first_lhs_error_index = i; }
    }
    lhs_qts.push_back(qt);
    if (not(qt.quals() >= type::Quals::Ref())) {
      // TODO log an error
    }
    if (qt.quals() >= type::Quals::Const()) {
      diag().Consume(diagnostic::AssigningToConstant{
          .to = qt.type(),
          // TODO set the range to point more directly to the things we care
          // about.
          .range = node->range(),
      });
    }
  }

  int first_rhs_error_index = -1;
  for (int i = 0; i < node->lhs().size(); ++i) {
    auto const *r = node->rhs()[i];
    auto qt = VerifyType(r);
    if (not qt.ok()) {
      if (first_lhs_error_index == -1) { first_lhs_error_index = i; }
    }
    rhs_qts.push_back(qt);
  }

  internal::QualTypeIterator lhs_iter(lhs_qts.begin());
  internal::QualTypeIterator rhs_iter(rhs_qts.begin());

  internal::QualTypeIterator const lhs_end(lhs_qts.end());
  internal::QualTypeIterator const rhs_end(rhs_qts.end());

  while (true) {
    if (lhs_iter == lhs_end or rhs_iter == rhs_end) { break; }

    // TODO deal with immovable and uncopyable types.
    type::Type const *lhs_type = (*lhs_iter).type();
    type::Type const *rhs_type = (*rhs_iter).type();
    if (not type::CanCast(lhs_type, rhs_type)) {
      diag().Consume(diagnostic::InvalidCast{
          .from = lhs_type,
          .to   = rhs_type,
          // TODO set the range to point more directly to the things we care
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
