#include "type/overload_set.h"

#include <string>
#include <vector>

#include "base/debug.h"
#include "base/guarded.h"

namespace type {
namespace {

base::guarded<std::vector<std::unique_ptr<OverloadSet>>> overload_sets;

}  // namespace

void OverloadSet::WriteTo(std::string *buf) const {
  buf->append("overload-set");
}

core::Bytes OverloadSet::bytes(core::Arch const &arch) const {
  return core::Bytes{0};
}

core::Alignment OverloadSet::alignment(core::Arch const &arch) const {
  return core::Alignment{1};
}

OverloadSet const *MakeOverloadSet(absl::flat_hash_set<Type> const &ts) {
  ASSERT(ts.size() != 0u);
  auto handle = overload_sets.lock();
  for (auto const &overload_set : *handle) {
    if (overload_set->members_.size() != ts.size()) { goto next_overload_set; }

    for (Type t : ts) {
      if (not overload_set->members_.contains(t)) { goto next_overload_set; }
    }

    return overload_set.get();

  next_overload_set:;
  }
  return handle->emplace_back(new OverloadSet(ts)).get();
}

}  // namespace type
