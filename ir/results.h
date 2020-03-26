#ifndef ICARUS_IR_RESULTS_H
#define ICARUS_IR_RESULTS_H

#include <type_traits>
#include <vector>

#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"
#include "core/bytes.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace ir {

struct Results {
 public:
  template <typename... Args>
  explicit Results(Args... args) {
    (append<Args>(args), ...);
  }

  static Results FromUntypedBuffer(std::vector<uint32_t> offsets,
                                   base::untyped_buffer buf);
  static Results FromRaw(void const* data, core::Bytes bytes);
  static Results FromRaw(base::untyped_buffer_view buf) {
    return FromRaw(buf.raw(0), core::Bytes(buf.size()));
  }

  template <typename T,
            typename = std::enable_if_t<not std::is_base_of_v<Reg, T>>>
  RegOr<T> get(size_t index) const {
    ASSERT(index < size());
    if constexpr (std::is_same_v<T, IsRegOr<T>>) {
      return get<typename IsRegOr<T>::type>(index);
    } else {
      auto offset = offset_[index];
      if (is_reg_[index]) { return buf_.get<Reg>(offset); }
      return buf_.get<T>(offset);
    }
  }

  bool is_reg(size_t index) const { return is_reg_[index]; }

  template <typename T, typename = std::enable_if_t<std::is_base_of_v<Reg, T>>>
  Reg get(size_t index) const {
    ASSERT(index < size());
    ASSERT(is_reg(index) == true);
    return buf_.get<Reg>(offset_[index]);
  }

  template <typename T>
  size_t append(T const& t) {
    if constexpr (std::is_same_v<Results, T>) {
      uint32_t current_buf_end = buf_.size();
      buf_.write(current_buf_end, t.buf_);
      is_reg_.insert(is_reg_.end(), t.is_reg_.begin(), t.is_reg_.end());
      offset_.reserve(offset_.size() + t.size());
      for (auto off : t.offset_) { offset_.push_back(current_buf_end + off); }
      return offset_.back();
    } else if constexpr (IsRegOr<T>::value) {
      return t.apply([&](auto v) { return append(v); });
    } else {
      is_reg_.push_back(std::is_base_of_v<Reg, T>);
      return offset_.emplace_back(buf_.append(t));
    }
  }

  // This is a strange thing to add as an API for the sole purpose of inlining.
  // Probably means this is the wrong abstraction.
  template <typename Fn>
  void for_each_reg(Fn f) {
    for (size_t i = 0; i < is_reg_.size(); ++i) {
      if (not is_reg_[i]) { continue; }
      Reg r = buf_.get<Reg>(offset_[i]);
      f(r);
      buf_.set(offset_[i], r);
    }
  }

  base::untyped_buffer extract_buffer() && { return std::move(buf_); }

  Results GetResult(size_t index) const;

  std::string to_string() const;

  size_t size() const { return offset_.size(); }
  size_t empty() const { return offset_.empty(); }

 private:
  std::vector<bool> is_reg_;
  std::vector<uint32_t> offset_;
  base::untyped_buffer buf_{16};
};
}  // namespace ir

#endif  // ICARUS_IR_RESULTS_H
