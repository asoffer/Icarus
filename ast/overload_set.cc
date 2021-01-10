#include "ast/overload_set.h"

#include "absl/container/flat_hash_set.h"
#include "ast/ast.h"
#include "ast/scope/scope.h"
#include "base/log.h"
#include "module/module.h"

namespace ast {

// TODO: Remove this overload
OverloadSet::OverloadSet(absl::Span<Declaration const *const> decls) {
  members_.reserve(decls.size());
  for (auto const *decl : decls) { members_.push_back(decl); }
}

OverloadSet::OverloadSet(absl::Span<Declaration::Id const *const> ids) {
  members_.reserve(ids.size());
  // TODO: Support multiple declarations
  for (auto const *id : ids) { members_.push_back(id); }
}

OverloadSet::OverloadSet(Scope const *scope, std::string_view id)
    : OverloadSet(module::AllDeclsTowardsRoot(scope, id)) {
  LOG("OverloadSet", R"(Constructing an overload set from "%s")", id);
}

}  // namespace ast
