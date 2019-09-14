#ifndef ICARUS_IR_RESULTS_H
#define ICARUS_IR_RESULTS_H

#include <type_traits>
#include <vector>

#include "base/untyped_buffer.h"
#include "ir/reg.h"
#include "ir/reg_or.h"
#include "core/bytes.h"

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

  template <typename T, typename = std::enable_if_t<!std::is_base_of_v<Reg, T>>>
  RegOr<T> get(size_t index) const {
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
      for (auto off : offset_) { offset_.push_back(current_buf_end + off); }
      return offset_.back();
    } else if constexpr (IsRegOr<T>::value) {
      return t.apply([&](auto v) { return append(v); });
    } else {
      is_reg_.push_back(std::is_base_of_v<Reg, T>);
      return offset_.emplace_back(buf_.append(t));
    }
  }

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
