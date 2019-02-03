#ifndef ICARUS_PROPERTY_PROPERTY_MAP_H
#define ICARUS_PROPERTY_PROPERTY_MAP_H

#include <unordered_set>

#include "base/container/unordered_map.h"
#include "base/hash.h"
#include "base/owned_ptr.h"
#include "base/util.h"
#include "ir/cmd.h"
#include "property/property.h"
#include "property/property_set.h"

namespace ir {
struct Arguments;
struct BasicBlock;
struct Func;
}  // namespace ir

namespace prop {
struct FnStateView {
  FnStateView(ir::Func *fn);

  base::unordered_map<ir::Register, PropertySet> view_;
};

inline std::ostream &operator<<(std::ostream &os, const FnStateView &fsv) {
  return os << base::internal::stringify(fsv.view_);
}

// TODO rename me.
struct Entry {
  Entry(const ir::BasicBlock *viewing_block, ir::Register reg)
      : viewing_block_(viewing_block), reg_(reg) {}

  const ir::BasicBlock *viewing_block_;
  ir::Register reg_;
};

inline std::ostream &operator<<(std::ostream &os, const Entry &e) {
  return os << "{" << e.viewing_block_ << ", " << e.reg_ << "}";
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
  PropertyMap(ir::Func *fn);
  PropertyMap(const PropertyMap &p) = default;
  PropertyMap &operator=(const PropertyMap &p) = default;
  PropertyMap(PropertyMap &&p) noexcept        = default;
  PropertyMap &operator=(PropertyMap &&p) noexcept = default;

  // Make a copy of this map and set the arguments to the values passed in
  PropertyMap with_args(ir::Arguments const &, FnStateView const &) const;

  // TODO rename or delete me.
  BoolProp Returns() const;

  PropertySet const &lookup(Entry const &e) const {
    return view_.at(e.viewing_block_).view_.at(e.reg_);
  }

  PropertySet &lookup(Entry const &e) {
    return view_.at(e.viewing_block_).view_.at(e.reg_);
  }

  PropertySet const &lookup(ir::BasicBlock const *viewing_block,
                            ir::Register reg) const {
    return view_.at(viewing_block).view_.at(reg);
  }

  PropertySet &lookup(ir::BasicBlock const *viewing_block, ir::Register reg) {
    return view_.at(viewing_block).view_.at(reg);
  }

  ir::Func *fn_ = nullptr;
  base::unordered_map<const ir::BasicBlock *, FnStateView> view_;

 private:
  void refresh(std::unordered_set<Entry> stale_up,
               std::unordered_set<Entry> stale_down);

  PropertyMap AssumingReturnsTrue() const;

  void MarkReferencesStale(Entry const &e,
                           std::unordered_set<Entry> *stale_down);
  bool UpdateEntryFromAbove(Entry const &e);
  void UpdateEntryFromBelow(Entry const &e, std::unordered_set<Entry> *stale_up,
                            std::unordered_set<Entry> *stale_down);
};

}  // namespace prop

#endif  // ICARUS_PROPERTY_PROPERTY_MAP_H
