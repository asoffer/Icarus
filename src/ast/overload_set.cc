#include "ast/overload_set.h"
#include "ast/declaration.h"
#include "context.h"
#include "scope.h"

namespace ast {
// TODO only hold functions?
OverloadSet::OverloadSet(Scope *scope, std::string const &id, Context *ctx) {
  auto decls = scope->AllDeclsWithId(id, ctx).first;
  reserve(decls.size());
  for (auto const &decl : decls) { emplace_back(decl.get(), decl.type()); }
}

using base::check::Is;
void OverloadSet::keep_return(type::Type const *t) {
  auto head_iter  = begin();
  auto tail_iter = end();
  --tail_iter;
  while (head_iter != tail_iter) {
    // TODO emit an error earlier if this happens for "as".
    ASSERT(head_iter->type(), Is<type::Function>());
    auto &fn_type = head_iter->type()->as<type::Function>();
    // TODO sohuld we support converting pairs?
    // For example, complex -> (float64, float64)?
    ASSERT(fn_type.output.size() == 1u);
    if (&fn_type == t) {
      ++head_iter;
    } else {
      *head_iter = std::move(*tail_iter);
      --tail_iter;
    }
  }
  ++tail_iter;
  this->erase(tail_iter, end());
}

}  // namespace ast
