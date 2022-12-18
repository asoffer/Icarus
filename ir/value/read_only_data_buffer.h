#ifndef ICARUS_IR_VALUE_READ_ONLY_DATA_BUFFER_H
#define ICARUS_IR_VALUE_READ_ONLY_DATA_BUFFER_H

#include <span>
#include <string>

#include "absl/container/flat_hash_map.h"

namespace ir {

struct ReadOnlyDataBuffer {
  size_t append(std::span<std::byte const> data) {
    auto [iter, inserted] = data_.emplace(to_view(data), data_.size());
    return iter->second;
  }

  size_t append_text(std::string text) {
    text.push_back('\0');
    auto [iter, inserted] = data_.emplace(std::move(text), data_.size());
    return iter->second;
  }

 private:
  static std::string_view to_view(std::span<std::byte const> data) {
    return std::string_view(reinterpret_cast<char const *>(data.data()),
                            data.size());
  }

  absl::flat_hash_map<std::string,size_t> data_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_READ_ONLY_DATA_BUFFER_H
