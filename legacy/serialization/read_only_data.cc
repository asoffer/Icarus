#include "serialization/read_only_data.h"

#include <utility>

#include "nth/debug/debug.h"

namespace serialization {

std::pair<size_t, bool> ReadOnlyData::insert(std::string const& content) {
  auto [iter, inserted] = data_.insert(content);
  return std::pair(data_.index(iter), inserted);
}

std::pair<size_t, bool> ReadOnlyData::insert(std::string&& content) {
  auto [iter, inserted] = data_.insert(std::move(content));
  return std::pair(data_.index(iter), inserted);
}

void ReadOnlyData::Serialize(ReadOnlyData const& from,
                             proto::ReadOnlyData& to) {
  for (auto const& s : from.data_) { *to.add_strings() = s; }
}

bool ReadOnlyData::Deserialize(proto::ReadOnlyData const& from,
                               ReadOnlyData& to) {
  for (auto const& s : from.strings()) {
    auto [iter, inserted] = to.data_.insert(s);
    if (not inserted) { return false; }
  }
  return true;
}

size_t ReadOnlyData::index(std::string const& s) const {
  auto iter = data_.find(s);
  NTH_ASSERT(iter != data_.end());
  return data_.index(data_.find(s));
}

std::string const& ReadOnlyData::string(size_t n) const {
  NTH_ASSERT(n < data_.size());
  return data_.from_index(n);
}

}  // namespace serialization
