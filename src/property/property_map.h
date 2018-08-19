#ifndef ICARUS_PROPERTY_PROPERTY_MAP_H
#define ICARUS_PROPERTY_PROPERTY_MAP_H

#include "base/container/stale.h"
#include "base/container/unordered_map.h"
#include "base/hash.h"
#include "base/owned_ptr.h"
#include "base/util.h"
#include "ir/cmd.h"
#include "property/property.h"
#include "property/property_set.h"

namespace IR {
struct BasicBlock;
struct Func;
}  // namespace IR

namespace prop {
struct FnStateView {
  FnStateView(IR::Func *fn);

  base::unordered_map<IR::Register, PropertySet> view_;
};

inline std::ostream &operator<<(std::ostream &os, const FnStateView &fsv) {
  return os << base::internal::stringify(fsv.view_);
}

// TODO rename me.
struct Entry {
  Entry(const IR::BasicBlock *viewing_block, IR::Register reg)
      : viewing_block_(viewing_block), reg_(reg) {}

  const IR::BasicBlock *viewing_block_;
  IR::Register reg_;
};

inline std::ostream &operator<<(std::ostream &os, const Entry &e) {
  return os << "{" << e.viewing_block_ << ", " << e.reg_.to_string() << "}";
}

inline bool operator==(const Entry &lhs, const Entry &rhs) {
  return lhs.viewing_block_ == rhs.viewing_block_ && lhs.reg_ == rhs.reg_;
}
}  // namespace prop

namespace std {
template <>
struct hash<prop::Entry> {
  size_t operator()(const prop::Entry &e) const {
    return base::hash_args(e.viewing_block_, e.reg_);
  }
};
}  // namespace std

namespace prop {
struct PropertyMap {
  PropertyMap() = default;
  PropertyMap(IR::Func *fn);
  PropertyMap(const PropertyMap &p) = default;
  PropertyMap &operator=(const PropertyMap &p) = default;
  PropertyMap(PropertyMap &&p) noexcept        = default;
  PropertyMap &operator=(PropertyMap &&p) noexcept = default;

  // Make a copy of this map and set the arguments to the values passed in
  PropertyMap with_args(IR::LongArgs const &, FnStateView const &) const;

  // TODO rename or delete me.
  BoolProp Returns() const;

  void refresh();

  IR::Func *fn_ = nullptr;
  // TODO given that you want the invariant that this is always empty...
  // probably shouldn't be storing it.
  base::stale_set<Entry> stale_down_;
  base::stale_set<Entry> stale_up_;
  base::unordered_map<const IR::BasicBlock *, FnStateView> view_;

 private:
  void MarkStale(Entry const &e);
  bool UpdateEntryFromAbove(Entry const &e);
  void UpdateEntryFromBelow(Entry const &e);
};

}  // namespace prop

#endif  // ICARUS_PROPERTY_PROPERTY_MAP_H
