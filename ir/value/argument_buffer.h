#ifndef ICARUS_IR_VALUE_ARGUMENT_BUFFER_H
#define ICARUS_IR_VALUE_ARGUMENT_BUFFER_H

#include <vector>

#include "base/meta.h"
#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"

namespace ir {
namespace internal_argument_buffer {

struct Offset {
  uint32_t index : 31;
  uint32_t is_register : 1;
};

}  // namespace internal_argument_buffer

struct ArgumentBuffer {
  template <typename T>
  void append(T const &value) {
    if constexpr (base::meta<T>.template is_a<ir::RegOr>()) {
      offsets_.push_back(internal_argument_buffer::Offset{
          .index       = static_cast<uint32_t>(buffer_.size()),
          .is_register = value.is_reg()});
      if (value.is_reg()) {
        buffer_.append(value.reg());
      } else {
        buffer_.append(value.value());
      }
    } else {
      offsets_.push_back(internal_argument_buffer::Offset{
          .index       = static_cast<uint32_t>(buffer_.size()),
          .is_register = false});
      buffer_.append(value);
    }
  }

  bool is_register(size_t i) const { return offsets_[i].is_register; }

  base::untyped_buffer_view operator[](size_t i) const {
    return base::untyped_buffer_view(
        buffer_.raw(offsets_[i].index),
        (i == offsets_.size() ? buffer_.size() : offsets_[i + 1].index) -
            offsets_[i].index);
  }

 private:
  std::vector<internal_argument_buffer::Offset> offsets_;
  base::untyped_buffer buffer_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_ARGUMENT_BUFFER_H
