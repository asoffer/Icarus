#ifndef ICARUS_IR_VALUE_RESULT_BUFFER_H
#define ICARUS_IR_VALUE_RESULT_BUFFER_H

#include <vector>

#include "base/meta.h"
#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"
#include "ir/value/reg_or.h"

namespace ir {
namespace internal_result_buffer {

struct Offset {
  uint32_t index : 31;
  uint32_t is_register : 1;
};

}  // namespace internal_result_buffer

struct PartialResultBuffer;
struct CompleteResultBuffer;

struct PartialResultRef {
  template <typename T>
  RegOr<T> get() const {
    if (is_register_) {
      return view_.get<T>(0);
    } else {
      return view_.get<RegOr<T>>(0);
    }
  }

  bool empty() const { return view_.empty(); }
  base::untyped_buffer_view raw() const { return view_; }

  bool is_register() const { return is_register_; }

 private:
  friend PartialResultBuffer;

  explicit PartialResultRef(base::untyped_buffer_view view, bool is_register)
      : view_(view), is_register_(is_register) {}

  base::untyped_buffer_view view_;
  bool is_register_;
};

struct CompleteResultRef {
  template <typename T>
  T get() const {
    return view_.get<T>(0);
  }

  bool empty() const { return view_.empty(); }
  base::untyped_buffer_view raw() const { return view_; }

 private:
  friend CompleteResultBuffer;

  explicit CompleteResultRef(base::untyped_buffer_view view) : view_(view) {}

  base::untyped_buffer_view view_;
};

struct CompleteResultBuffer {
  void clear() {
    buffer_.clear();
    offsets_.clear();
  }

  void append(CompleteResultBuffer const &value);
  void append(CompleteResultRef const &value);

  template <typename T>
  void append(T const &value) {
    offsets_.push_back(buffer_.size());
    buffer_.append(value);
  }

  void append();

  base::untyped_buffer buffer() && { return std::move(buffer_); }

  bool empty() const { return offsets_.empty(); }

  template <typename T>
  T get(size_t i) const {
    return (*this)[i].get<T>();
  }

  CompleteResultRef operator[](size_t i) const;
  CompleteResultRef back() const;

 private:
  friend PartialResultBuffer;

  std::vector<size_t> offsets_;
  base::untyped_buffer buffer_;
};

struct PartialResultBuffer {
  PartialResultBuffer() = default;
  PartialResultBuffer(CompleteResultBuffer buffer);

  void clear() {
    buffer_.clear();
    offsets_.clear();
  }

  void append(PartialResultBuffer const &value);
  void append(CompleteResultBuffer const &value);
  void append(PartialResultRef const &value);
  void append(CompleteResultRef const &value);

  template <typename T>
  void append(T const &value) {
    if constexpr (base::meta<T>.template is_a<RegOr>()) {
      offsets_.push_back(internal_result_buffer::Offset{
          .index       = static_cast<uint32_t>(buffer_.size()),
          .is_register = value.is_reg()});
      if (value.is_reg()) {
        buffer_.append(value.reg());
      } else {
        buffer_.append(value.value());
      }
    } else if constexpr (base::meta<T> == base::meta<Reg>) {
      offsets_.push_back(internal_result_buffer::Offset{
          .index = static_cast<uint32_t>(buffer_.size()), .is_register = true});
      buffer_.append(value);
    } else {
      offsets_.push_back(internal_result_buffer::Offset{
          .index       = static_cast<uint32_t>(buffer_.size()),
          .is_register = false});
      buffer_.append(value);
    }
  }

  void append();

  bool is_register(size_t i) const { return offsets_[i].is_register; }

  base::untyped_buffer buffer() && { return std::move(buffer_); }

  bool empty() const { return offsets_.empty(); }

  template <typename T>
  RegOr<T> get(size_t i) const {
    return (*this)[i].get<T>();
  }

  PartialResultRef operator[](size_t i) const;
  PartialResultRef back() const;

 private:
  std::vector<internal_result_buffer::Offset> offsets_;
  base::untyped_buffer buffer_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_RESULT_BUFFER_H
