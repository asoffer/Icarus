#ifndef ICARUS_MODULE_GLOBAL_ENUM_MAP_H
#define ICARUS_MODULE_GLOBAL_ENUM_MAP_H

#include <deque>
#include <utility>

#include "absl/container/flat_hash_map.h"

namespace module {

struct EnumEntry {
  explicit EnumEntry(std::vector<std::pair<std::string, uint64_t>> values)
      : values_(std::move(values)) {}

  std::span<std::pair<std::string, uint64_t> const> values() const {
    return values_;
  }

  bool has_member(std::string_view name) const {
    auto iter = std::lower_bound(values_.begin(), values_.end(), name,
                                 [](auto const& pair, std::string_view name) {
                                   return pair.first < name;
                                 });
    return iter != values_.end() and iter->first == name;
  }

  std::optional<uint64_t> value(std::string_view name) const {
    auto iter = std::lower_bound(values_.begin(), values_.end(), name,
                                 [](auto const& pair, std::string_view name) {
                                   return pair.first < name;
                                 });
    if (iter == values_.end() or iter->first != name) { return std::nullopt; }
    return iter->second;
  }

 private:
  std::vector<std::pair<std::string, uint64_t>> values_;
};

struct GlobalEnumMap {
  size_t insert(std::vector<std::pair<std::string, uint64_t>> values) {
    size_t index = entries_.size();
    entries_.emplace_back(std::move(values));
    return index;
  }

  auto begin() const { return entries_.begin(); }
  auto end() const { return entries_.end(); }

  EnumEntry const& entry(size_t index) { return entries_[index]; }

 private:
  std::deque<EnumEntry> entries_;
};

}  // namespace module

#endif  // ICARUS_MODULE_GLOBAL_ENUM_MAP_H
