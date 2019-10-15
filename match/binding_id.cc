#include "match/binding_id.h"

#include "absl/container/node_hash_set.h"
#include "base/guarded.h"

namespace match {

static base::guarded<absl::node_hash_set<std::string>> intern_set;
BindingId::BindingId(std::string_view name) {
  auto handle = intern_set.lock();
  auto iter   = handle->lazy_emplace(
      name, [&](auto const& ctor) { ctor(std::string(name)); });
  name_ = *iter;
}

}  // namespace match
