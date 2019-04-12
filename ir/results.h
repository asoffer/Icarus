#ifndef ICARUS_IR_RESULTS_H
#define ICARUS_IR_RESULTS_H

#include <type_traits>
#include <vector>

#include "base/untyped_buffer.h"
#include "ir/register.h"
#include "core/bytes.h"

namespace ir {
struct Val;

struct Results {
 public:
  template <typename... Args>
  explicit Results(Args... args) {
    (append<Args>(args), ...);
  }

  static Results FromUntypedBuffer(std::vector<int64_t> offsets,
                                   base::untyped_buffer buf);
  static Results FromRaw(void const* data, core::Bytes bytes);

  template <typename T, typename = std::enable_if_t<!std::is_base_of_v<Reg, T>>>
  RegisterOr<T> get(size_t index) const {
    if constexpr (std::is_same_v<T, IsRegOr<T>>) {
      return get<typename IsRegOr<T>::type>(index);
    } else {
      auto offset = offset_[index];
      if (offset > 0) { return Reg{static_cast<uint64_t>(offset - 1)}; }
      return buf_.get<T>(-offset);
    }
  }

  bool is_reg(size_t index) const { return offset_[index] > 0; }

  template <typename T, typename = std::enable_if_t<std::is_base_of_v<Reg, T>>>
  Reg get(size_t index) const {
    return Reg{static_cast<uint64_t>(offset_[index] - 1)};
  }

  template <typename T>
  size_t append(T const& t) {
    if constexpr (std::is_same_v<Results, T>) {
      int64_t current_buf_end = buf_.size();
      buf_.write(buf_.size(), t.buf_);
      offset_.reserve(offset_.size() + t.offset_.size());
      for (int64_t off : t.offset_) {
        offset_.push_back(off <= 0 ? off - current_buf_end : off);
      }
      return offset_.back();
    } else if constexpr (std::is_base_of_v<Reg, T>) {
      return offset_.emplace_back(1 + static_cast<int64_t>(t.value()));
    } else if constexpr (IsRegOr<T>::value) {
      return t.is_reg_ ? append(t.reg_) : append(t.val_);
    } else {
      return offset_.emplace_back(-static_cast<int64_t>(buf_.append(t)));
    }
  }

  Results GetResult(size_t index) const;

  std::string to_string() const;

  size_t size() const { return offset_.size(); }
  size_t empty() const { return offset_.empty(); }

 private:
  // TODO reduce indicetions by storing registers directly in the vector. Be
  // sure to handle their bit patterns correctly.
  //
  // Value at position `n` is the offset of the `n`th entry in `buf_`. Negative
  // values indicate that the value stored is a register rather than the actual
  // value.
  std::vector<int64_t> offset_;
  base::untyped_buffer buf_{16};
};
}  // namespace ir

#endif  // ICARUS_IR_RESULTS_H
