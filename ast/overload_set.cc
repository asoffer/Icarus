#include "ast/overload_set.h"
#include "ast/declaration.h"
#include "misc/context.h"
#include "core/scope.h"

namespace ast {
using ::matcher::InheritsFrom;

// TODO only hold functions?
OverloadSet::OverloadSet(core::Scope *scope, std::string const &id, Context *ctx) {
  auto decls = scope->AllDeclsWithId(id, ctx);
  reserve(decls.size());
  for (auto const &decl : decls) { emplace_back(decl.get(), decl.type()); }
}

void OverloadSet::keep_return(type::Type const *t) {
  auto head_iter = begin();
  auto tail_iter = end();
  --tail_iter;
  while (head_iter != tail_iter) {
    // TODO emit an error earlier if this happens for "as".
    ASSERT(head_iter->type(), InheritsFrom<type::Function>());
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

void OverloadSet::add_adl(std::string const &id, type::Type const *t) {
  std::unordered_set<::Module const *> modules;
  t->defining_modules(&modules);

  for (auto *mod : modules) {
    ASSIGN_OR(continue, auto &d, mod->GetDecl(id));
    ASSIGN_OR(continue, auto &t, mod->GetType(id));
    // TODO handle this case. I think it's safe to just discard it.
    ASSERT(&t, InheritsFrom<type::Callable>());

    if (std::none_of(this->begin(), this->end(),
                     [&d](auto const &expr) { return &d == expr.get(); })) {
      emplace_back(&d, &t);
    }
  }
}

}  // namespace ast
