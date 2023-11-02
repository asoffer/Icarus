#ifndef ICARUS_TYPE_BYTE_WIDTH_H
#define ICARUS_TYPE_BYTE_WIDTH_H

#include <compare>
#include <cstdint>

#include "type/alignment.h"

namespace ic::type {

struct ByteWidth {
  explicit constexpr ByteWidth(uint32_t n) : width_(n) {}
  constexpr uint32_t value() const { return width_; }

  friend auto operator<=>(ByteWidth, ByteWidth) = default;

  friend void NthPrint(auto& p, auto& f, ByteWidth b) {
    f(p, b.value());
    p.write(" bytes");
  }

  ByteWidth aligned_forward_to(Alignment a) const;
  ByteWidth aligned_backward_to(Alignment a) const;
  void align_forward_to(Alignment a);
  void align_backward_to(Alignment a);

 private:
  uint32_t width_;
};

}  // namespace ic::type

#endif  // ICARUS_TYPE_BYTE_WIDTH_H
