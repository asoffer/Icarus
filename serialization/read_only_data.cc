#include "serialization/read_only_data.h"

#include "base/debug.h"

namespace serialization {

void ReadOnlyDataAggregator::merge(ModuleIndex module_index,
                                   ReadOnlyData const& data) {
  size_t i = 0;
  for (std::string const& s : data.strings()) { insert(module_index, i++, s); }
}

void ReadOnlyDataAggregator::insert(ModuleIndex module_index, size_t index,
                                    std::string const& value) {
  auto [iter, inserted] = data_.insert(value);
  mapping_.emplace(std::pair(module_index, index), data_.index(iter));
}

void ReadOnlyDataAggregator::write_to(ReadOnlyData& output) {
  for (auto const& s : data_) { *output.add_strings() = s; }
}

std::pair<size_t, std::string_view> ReadOnlyDataAggregator::read(
    ModuleIndex module_index, size_t index) const {
  auto iter = mapping_.find(std::pair(module_index, index));
  ASSERT(iter != mapping_.end());
  return std::pair<size_t, std::string_view>(iter->second,
                                             data_.from_index(iter->second));
}

}  // namespace serialization
