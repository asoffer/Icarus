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

struct ArgumentBuffer;

struct ArgumentRef {
  template <typename T>
  RegOr<T> get() const {
    return view_.get<RegOr<T>>(0);
  }

  bool empty() const { return view_.empty(); }
  base::untyped_buffer_view raw() const { return view_; }

 private:
  friend ArgumentBuffer;

  explicit ArgumentRef(base::untyped_buffer_view view) : view_(view) {}

  base::untyped_buffer_view view_;
};

struct ArgumentBuffer {
  void append(ArgumentBuffer const &value) {
    size_t size = buffer_.size();
    offsets_.reserve(offsets_.size() + value.offsets_.size());
    for (internal_argument_buffer::Offset o : value.offsets_) {
      o.index += size;
      offsets_.push_back(o);
    }
    buffer_.write(buffer_.size(), value.buffer_);
  }

  template <typename T>
  void append(T const &value) {
    if constexpr (base::meta<T>.template is_a<RegOr>()) {
      offsets_.push_back(internal_argument_buffer::Offset{
          .index       = static_cast<uint32_t>(buffer_.size()),
          .is_register = value.is_reg()});

      buffer_.append(value);
      // TODO:
      // if (value.is_reg()) {
      //   buffer_.append(value.reg());
      // } else {
      //   buffer_.append(value.value());
      // }
    } else {
      offsets_.push_back(internal_argument_buffer::Offset{
          .index       = static_cast<uint32_t>(buffer_.size()),
          .is_register = false});
      buffer_.append(RegOr(value));
    }
  }

  void append() {
    offsets_.push_back(
        offsets_.empty()
            ? internal_argument_buffer::Offset{.index = 0, .is_register = 0}
            : offsets_.back());
  }

  bool is_register(size_t i) const { return offsets_[i].is_register; }

  base::untyped_buffer buffer() && { return std::move(buffer_); }

  template <typename T>
  RegOr<T> get(size_t i) const {
    return (*this)[i].get<T>();
  }

  ArgumentRef operator[](size_t i) const {
    ASSERT(i < offsets_.size());
    return ArgumentRef(base::untyped_buffer_view(
        buffer_.raw(offsets_[i].index),
        (i + 1 == offsets_.size() ? buffer_.size() : offsets_[i + 1].index) -
            offsets_[i].index));
  }

 private:
  std::vector<internal_argument_buffer::Offset> offsets_;
  base::untyped_buffer buffer_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_ARGUMENT_BUFFER_H
