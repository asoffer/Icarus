#include "type/overload_set.h"

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

std::vector<type::Type> OverloadSet::return_types(
    core::Arguments<type::Typed<ir::Value>> const &args) const {
  ASSERT(callables_.size() == 1u);  // TODO: Support dynamic disptach
  return (*callables_.begin())->return_types(args);
}

Callable const *MakeOverloadSet(
    absl::flat_hash_set<Callable const *> const &cs) {
  ASSERT(cs.size() != 0u);
  if (cs.size() == 1) {
    return *cs.begin();
  } else {
    auto handle = overload_sets.lock();
    for (auto const &overload_set : *handle) {
      if (overload_set->callables_.size() != cs.size()) {
        goto next_overload_set;
      }
      for (Callable const *c : cs) {
        if (c->if_as<type::OverloadSet>()) {
          NOT_YET("Trying to make a nested overload set");
        } else {
          if (not overload_set->callables_.contains(c)) {
            goto next_overload_set;
          }
        }
      }

      return overload_set.get();

    next_overload_set:;
    }
    return handle->emplace_back(new OverloadSet(cs)).get();
  }
}

Callable const *MakeOverloadSet(absl::flat_hash_set<Callable const *> &&cs) {
  ASSERT(cs.size() != 0u);
  if (cs.size() == 1) {
    return *cs.begin();
  } else {
    auto handle = overload_sets.lock();
    for (auto const &overload_set : *handle) {
      if (overload_set->callables_.size() != cs.size()) {
        goto next_overload_set;
      }
      for (Callable const *c : cs) {
        if (not overload_set->callables_.contains(c)) {
          goto next_overload_set;
        }
      }

      return overload_set.get();

    next_overload_set:;
    }
    return handle->emplace_back(new OverloadSet(std::move(cs))).get();
  }
}

}  // namespace type
