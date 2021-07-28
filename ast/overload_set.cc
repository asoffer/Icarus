#include "ast/overload_set.h"

#include "absl/container/flat_hash_set.h"
#include "ast/ast.h"
#include "ast/scope.h"
#include "base/log.h"
#include "module/module.h"

namespace ast {

OverloadSet::OverloadSet(absl::Span<Declaration::Id const *const> ids) {
  members_.reserve(ids.size());
  for (auto const *id : ids) { members_.push_back(id); }
}

OverloadSet::OverloadSet(Scope const *scope, std::string_view name) {
  scope->ForEachDeclIdTowardsRoot(name, [&](Declaration::Id const *id) {
    members_.push_back(id);
    return true;
  });
  LOG("OverloadSet", R"(Constructing an overload set from "%s")", name);
}

}  // namespace ast
