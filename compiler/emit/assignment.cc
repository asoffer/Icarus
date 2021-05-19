#include <vector>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::Assignment const *node) {
  // This first case would be covered by the general case, but this allows us to
  // avoid unnecessary temporary allocations when we know they are not
  // necessary.
  if (node->lhs().size() == 1) {
    ASSERT(node->rhs().size() == 1u);
    auto const *l = node->lhs()[0];
    type::Typed<ir::RegOr<ir::addr_t>> ref(
        EmitRef(l), context().qual_types(l)[0].type());
    EmitMoveAssign(node->rhs()[0], absl::MakeConstSpan(&ref, 1));
    return ir::Value();
  }

  std::vector<type::Typed<ir::RegOr<ir::addr_t>>> lhs_refs;
  lhs_refs.reserve(node->lhs().size());

  std::vector<type::Typed<ir::RegOr<ir::addr_t>>> temps;
  temps.reserve(node->lhs().size());

  // TODO: Understand the precise semantics you care about here and document
  // them. Must references be computed first?
  for (auto const *l : node->lhs()) {
    type::Type t = context().qual_types(l)[0].type();
    lhs_refs.emplace_back(EmitRef(l), t);
    temps.emplace_back(builder().TmpAlloca(t), t);
  }

  auto temp_iter = temps.begin();
  for (auto const *r : node->rhs()) {
    size_t num_rets = context().qual_types(r).size();
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> temp_span(&*temp_iter,
                                                                 num_rets);
    EmitMoveAssign(r, temp_span);
    temp_iter += num_rets;
  }

  for (auto temp_iter = temps.begin(), ref_iter = lhs_refs.begin();
       temp_iter != temps.end(); ++temp_iter, ++ref_iter) {
    EmitMoveAssign(
        *ref_iter,
        type::Typed<ir::Value>(
            ir::Value(builder().PtrFix((*temp_iter)->reg(), temp_iter->type())),
            temp_iter->type()));
  }
  return ir::Value();
}

}  // namespace compiler
