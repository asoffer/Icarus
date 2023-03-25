#ifndef ICARUS_VM_ARGUMENT_SLICE_H
#define ICARUS_VM_ARGUMENT_SLICE_H

#include <cstdint>

namespace vm {

struct ArgumentSlice {
  explicit ArgumentSlice(void* data, uint64_t length)
      : data_(data), length_(length) {}

  void* data() const { return data_; }
  uint64_t length() const { return length_; }

 private:
  void* data_;
  uint64_t length_;
};

}  // namespace vm

#endif  // ICARUS_VM_ARGUMENT_SLICE_H
