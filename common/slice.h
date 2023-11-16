#ifndef ICARUS_COMMON_SLICE_H
#define ICARUS_COMMON_SLICE_H

#include <cstddef>
#include <cstdint>

namespace ic {

// A data structure representing an Icarus "slice," guaranteed to have the same
// ABI as specified by the Icarus language.
struct Slice {
  explicit Slice(std::byte const *data, uint64_t count)
      : data_(data), count_(count) {}

  std::byte const *data() const { return data_; }
  uint64_t count() const { return count_; }

 private:
  std::byte const *data_;
  uint64_t count_;
};

}  // namespace ic

#endif  // ICARUS_COMMON_SLICE_H
