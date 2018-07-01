#ifndef ICARUS_PROPERTY_PROPERTY_MAP_H
#define ICARUS_PROPERTY_PROPERTY_MAP_H

#include <memory>
#include <unordered_map>
#include <vector>

#include "base/stale_set.h"
#include "ir/val.h"

namespace IR {
struct BasicBlock;
struct Func;
}  // namespace IR

namespace prop {
struct Property {
  virtual ~Property() {}
};

struct DefaultBoolProperty : public Property {
  DefaultBoolProperty() : can_be_true_(true), can_be_false_(true) {}
  DefaultBoolProperty(bool b) : can_be_true_(b), can_be_false_(!b) {}
  ~DefaultBoolProperty() override {}
  bool can_be_true_  = true;
  bool can_be_false_ = true;

  void Merge(const DefaultBoolProperty &p);
};

struct BlockStateView {
  BlockStateView(const IR::BasicBlock *block);
  std::vector<std::unique_ptr<Property>> view_;
};

struct FnStateView {
  FnStateView(IR::Func *fn);

  std::unordered_map<const IR::BasicBlock *, BlockStateView> view_;
};

struct Entry {
  Entry(const IR::BasicBlock *viewing_block, IR::CmdIndex cmd_index)
      : viewing_block_(viewing_block), cmd_index_(cmd_index) {}

  const IR::BasicBlock *viewing_block_;
  IR::CmdIndex cmd_index_;
};

inline bool operator==(const Entry &lhs, const Entry &rhs) {
  return lhs.viewing_block_ == rhs.viewing_block_ &&
         lhs.cmd_index_ == rhs.cmd_index_;
}
}  // namespace prop

namespace std {
template <>
struct hash<prop::Entry> {
  size_t operator()(const prop::Entry &e) const {
    size_t seed = 0x9e3779b97f681513ull;
    base::hash_combine(seed, e.viewing_block_);
    base::hash_combine(seed, e.cmd_index_.block);
    base::hash_combine(seed, e.cmd_index_.cmd);
    return seed;
  }
};
}  // namespace std

namespace prop {
struct PropertyMap {
  PropertyMap() = default;
  PropertyMap(IR::Func *fn);
  PropertyMap(PropertyMap &&p) noexcept = default;
  PropertyMap &operator=(PropertyMap &&p) noexcept = default;

  void Returns() const;

  IR::Func *fn_ = nullptr;
  base::stale_set<Entry> stale_entries_;
  std::unordered_map<const IR::BasicBlock *, FnStateView> view_;
};
}  // namespace prop

#endif  // ICARUS_PROPERTY_PROPERTY_MAP_H
