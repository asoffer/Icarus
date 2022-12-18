#ifndef ICARUS_IR_VALUE_SERIALIZED_POINTER_H
#define ICARUS_IR_VALUE_SERIALIZED_POINTER_H

#include <span>

namespace ir {

struct SerializedPointer {
  template <typename T>
  static SerializedPointer ReadOnly(
      ReadOnlyDataBuffer const &read_only_contents, T const *p) {
    ptrdiff_t offset =
        static_cast<std::byte const *>(p) - read_only_contents.data();
    SerializedPointer s;
    s.value_ = static_cast<uintptr_t>(offset);
    return s;
  }

  uintptr_t value_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_SERIALIZED_POINTER_H
