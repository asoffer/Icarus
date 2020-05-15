#ifndef ICARUS_BASE_FLYWEIGHT_MAP_H
#define ICARUS_BASE_FLYWEIGHT_MAP_H

#include <type_traits>
#include <vector>

#include "absl/container/node_hash_map.h"
#include "base/debug.h"

namespace base {

template <typename Data>
struct flyweight_map {
  static_assert(not std::is_same_v<Data, size_t>);
  size_t get(Data const &data) {
    auto [iter, inserted] = ids_.try_emplace(data);
    if (inserted) {
      size_t id    = ids_.size() - 1;
      iter->second = id;
      data_.push_back(&iter->first);
    }
    return iter->second;
  }

  Data const &get(size_t id) {
    ASSERT(id >= 0);
    ASSERT(id < data_.size());
    return *data_[id];
  }

 private:
  absl::node_hash_map<Data, size_t> ids_;
  std::vector<Data const *> data_;
};

}  // namespace base

#endif  // ICARUS_BASE_FLYWEIGHT_MAP_H
