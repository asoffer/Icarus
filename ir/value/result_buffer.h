#ifndef ICARUS_IR_VALUE_RESULT_BUFFER_H
#define ICARUS_IR_VALUE_RESULT_BUFFER_H

#include <vector>

#include "base/meta.h"
#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"
#include "ir/value/reg_or.h"
#include "ir/value/addr.h"

namespace ir {
namespace internal_result_buffer {

struct UnknownTag {};
#if defined(ICARUS_DEBUG)
inline constexpr bool kResultBufferDebug = true;
#else
inline constexpr bool kResultBufferDebug = false;
#endif  // defined(ICARUS_DEBUG)

struct Offset {
  uint32_t index : 31;
  uint32_t is_register : 1;
};

}  // namespace internal_result_buffer

struct PartialResultBuffer;
struct CompleteResultBuffer;
struct CompleteResultRef;

struct PartialResultRef {
  PartialResultRef() = default;

  template <typename T>
  auto get() const {
    if constexpr (base::meta<T> == base::meta<Reg>) {
      ASSERT(is_register_ == true);

      if constexpr (internal_result_buffer::kResultBufferDebug) {
        if (view_.get<base::MetaValue>(0) !=
            base::meta<internal_result_buffer::UnknownTag>) {
          ASSERT(view_.get<base::MetaValue>(0) == base::meta<Reg>);
        }
      }
      return Reg(view_.get<Reg>(sizeof(base::MetaValue) *
                                internal_result_buffer::kResultBufferDebug));
    } else {
      if (is_register_) {
        if constexpr (internal_result_buffer::kResultBufferDebug) {
          if (view_.get<base::MetaValue>(0) !=
              base::meta<internal_result_buffer::UnknownTag>) {
            ASSERT(view_.get<base::MetaValue>(0) == base::meta<Reg>);
          }
        }
        return RegOr<T>(
            view_.get<Reg>(sizeof(base::MetaValue) *
                           internal_result_buffer::kResultBufferDebug));
      } else {
        if constexpr (internal_result_buffer::kResultBufferDebug) {
          if (view_.get<base::MetaValue>(0) !=
              base::meta<internal_result_buffer::UnknownTag>) {
            ASSERT(view_.get<base::MetaValue>(0) == base::meta<T>);
          }
        }

        return RegOr<T>(
            view_.get<T>(sizeof(base::MetaValue) *
                         internal_result_buffer::kResultBufferDebug));
      }
    }
  }

  bool empty() const { return view_.empty(); }
  base::untyped_buffer_view raw() const {
    return base::untyped_buffer_view(
        view_.data() + sizeof(base::MetaValue) *
                           internal_result_buffer::kResultBufferDebug,
        view_.size() - sizeof(base::MetaValue) *
                           internal_result_buffer::kResultBufferDebug);
  }

  bool is_register() const { return is_register_; }

 private:
  friend PartialResultBuffer;
  friend CompleteResultRef;

  explicit PartialResultRef(base::untyped_buffer_view view, bool is_register)
      : view_(view), is_register_(is_register) {}

  base::untyped_buffer_view view_;
  bool is_register_;
};

struct CompleteResultRef {
  CompleteResultRef() = default;

  template <typename T>
  T get() const {
    if constexpr (internal_result_buffer::kResultBufferDebug) {
      if (view_.get<base::MetaValue>(0) !=
          base::meta<internal_result_buffer::UnknownTag>) {
        ASSERT(view_.get<base::MetaValue>(0) == base::meta<T>);
      }
    }

    return view_.get<T>(sizeof(base::MetaValue) *
                        internal_result_buffer::kResultBufferDebug);
  }

  bool empty() const { return view_.empty(); }
  base::untyped_buffer_view raw() const {
    return base::untyped_buffer_view(
        view_.data() + sizeof(base::MetaValue) *
                           internal_result_buffer::kResultBufferDebug,
        view_.size() - sizeof(base::MetaValue) *
                           internal_result_buffer::kResultBufferDebug);
  }

  operator PartialResultRef() const { return PartialResultRef(view_, false); }

 private:
  friend CompleteResultBuffer;

  explicit CompleteResultRef(base::untyped_buffer_view view) : view_(view) {}

  base::untyped_buffer_view view_;
};

struct CompleteResultBuffer {
  void reserve_bytes(size_t num_entries, size_t num_bytes);

  addr_t append_slot(size_t slot_size);

  void clear() {
    buffer_.clear();
    offsets_.clear();
  }

  void append(CompleteResultBuffer const &value);
  void append(CompleteResultRef value);

  template <typename T>
  void append(T const &value) {
    static_assert(not base::HasErasureWrapper<T>);
    offsets_.push_back(buffer_.size());

    if constexpr (internal_result_buffer::kResultBufferDebug) {
      buffer_.append(base::meta<T>.value());
    }
    buffer_.append(value);
  }

  void append();

  base::untyped_buffer buffer() && { return std::move(buffer_); }

  bool empty() const { return offsets_.empty(); }


  friend bool operator==(CompleteResultBuffer const &lhs,
                         CompleteResultBuffer const &rhs);

  friend bool operator!=(CompleteResultBuffer const &lhs,
                         CompleteResultBuffer const &rhs) {
    return not(lhs == rhs);
  }

  template <typename H>
  friend H AbslHashValue(H h, CompleteResultBuffer const &b) {
    h = H::combine(std::move(h), b.offsets_);
    return H::combine_contiguous(std::move(h), b.buffer_.data(),
                                 b.buffer_.size());
  }

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
  void append(PartialResultRef value);
  void append(CompleteResultRef value) {
    append(static_cast<PartialResultRef>(value));
  }

  template <typename T>
  void append(T const &value) {
    if constexpr (base::meta<T>.template is_a<RegOr>()) {
      static_assert(not base::HasErasureWrapper<typename T::type>);
      offsets_.push_back(internal_result_buffer::Offset{
          .index       = static_cast<uint32_t>(buffer_.size()),
          .is_register = value.is_reg()});
      if (value.is_reg()) {
        if constexpr (internal_result_buffer::kResultBufferDebug) {
          buffer_.append(base::meta<Reg>.value());
        }
        buffer_.append(value.reg());
      } else {
        if constexpr (internal_result_buffer::kResultBufferDebug) {
          buffer_.append(base::meta<typename T::type>.value());
        }
        buffer_.append(value.value());
      }
    } else if constexpr (base::meta<T> == base::meta<Reg>) {
      offsets_.push_back(internal_result_buffer::Offset{
          .index = static_cast<uint32_t>(buffer_.size()), .is_register = true});
      if constexpr (internal_result_buffer::kResultBufferDebug) {
        buffer_.append(base::meta<Reg>.value());
      }
      buffer_.append(value);
    } else {
      static_assert(not base::HasErasureWrapper<T>);
      offsets_.push_back(internal_result_buffer::Offset{
          .index       = static_cast<uint32_t>(buffer_.size()),
          .is_register = false});
      if constexpr (internal_result_buffer::kResultBufferDebug) {
        buffer_.append(base::meta<T>.value());
      }
      buffer_.append(value);
    }
  }

  void append();

  bool is_register(size_t i) const { return offsets_[i].is_register; }

  base::untyped_buffer buffer() && { return std::move(buffer_); }

  bool empty() const { return offsets_.empty(); }

  template <typename T>
  auto get(size_t i) const {
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
