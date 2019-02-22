#ifndef ICARUS_IR_RESULTS_H
#define ICARUS_IR_RESULTS_H

#include <type_traits>
#include <vector>

#include "base/untyped_buffer.h"
#include "ir/register.h"

namespace ir {
struct Val;

struct Results {
 public:
  template <typename... Args>
  explicit Results(Args... args) {
    (append<Args>(args), ...);
  }

  static Results FromVals(std::vector<Val> const& vals);

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
  Register get(size_t index) const {
    return Reg{static_cast<uint64_t>(offset_[index] - 1)};
  }

  template <typename T>
  size_t append(T const& t) {
    if constexpr (std::is_same_v<Results, T>) {
      buf_.write(buf_.size(), t.buf_);
      offset_.insert(offset_.end(), t.offset_.begin(), t.offset_.end());
      return offset_.back();
    } else if constexpr (std::is_base_of_v<Reg, T>) {
      return offset_.emplace_back(1 + static_cast<int64_t>(t.value()));
    } else if constexpr (IsRegOr<T>::value) {
      return t.is_reg_ ? append(t.reg_) : append(t.val_);
    } else {
      return offset_.emplace_back(-static_cast<int64_t>(buf_.append(t)));
    }
  }

  size_t size() const { return offset_.size(); }

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
