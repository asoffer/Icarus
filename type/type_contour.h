#ifndef ICARUS_TYPE_CONTOUR_H
#define ICARUS_TYPE_CONTOUR_H

#include <cstdint>

#include "type/alignment.h"
#include "type/byte_width.h"

namespace ic::type {

struct TypeContour {
  explicit constexpr TypeContour(ByteWidth width, Alignment a)
      : width_(width), alignment_(a) {}
  constexpr ByteWidth byte_width() const { return width_; }
  constexpr Alignment alignment() const { return alignment_; }

  friend bool operator==(TypeContour, TypeContour) = default;

 private:
  ByteWidth width_;
  Alignment alignment_;
};

}  // namespace ic::type

#endif  // ICARUS_TYPE_CONTOUR_H
