#ifndef ICARUS_IR_VALUE_SLICE_H
#define ICARUS_IR_VALUE_SLICE_H

#include <iostream>

#include "ir/value/addr.h"

namespace ir {

struct Slice {
  Slice() = default;
  explicit Slice(ir::Addr data, uint64_t length)
      : data_(data), length_(length) {}

  Addr data() const { return data_; }
  uint64_t length() const { return length_; }

  auto operator<=>(Slice const&) const = default;

  template <typename H>
  friend H AbslHashValue(H h, Slice s) {
    return H::combine(std::move(h), s.data(), s.length());
  }

  friend std::ostream& operator<<(std::ostream& os, Slice s) {
    return os << "Slice(" << s.data() << ", " << s.length() << ")";
  }

 private:
  ir::Addr data_;
  uint64_t length_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_SLICE_H
