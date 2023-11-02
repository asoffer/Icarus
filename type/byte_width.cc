#include "type/byte_width.h"

namespace ic::type {

ByteWidth ByteWidth::aligned_forward_to(Alignment a) const {
  auto copy = *this;
  copy.align_forward_to(a);
  return copy;
}

ByteWidth ByteWidth::aligned_backward_to(Alignment a) const {
  auto copy = *this;
  copy.align_backward_to(a);
  return copy;
}

void ByteWidth::align_forward_to(Alignment a) {
  width_ = ((width_ - 1) | (static_cast<uint32_t>(a.value()) - uint32_t{1})) +
           uint32_t{1};
}

void ByteWidth::align_backward_to(Alignment a) {
  width_ &= ~(static_cast<uint32_t>(a.value()) - uint32_t{1});
}

}  // namespace ic::type
