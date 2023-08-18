#ifndef ICARUS_CORE_TYPE_CONTOUR_H
#define ICARUS_CORE_TYPE_CONTOUR_H

#include "core/alignment.h"
#include "core/bytes.h"

namespace core {

// `TypeContour` represents the binary-level requirements for a type, including
// its size in bytes, and required alignment.
struct TypeContour {
  constexpr explicit TypeContour() : size_(0), alignment_(1) {}
  constexpr explicit TypeContour(Bytes b, Alignment a)
      : size_(b), alignment_(a) {}

  // Constructs a TypeCounter for the given type on the host architechture.
  template <typename T>
  static constexpr TypeContour Get() {
    return TypeContour(Bytes::Get<T>(), Alignment::Get<T>());
  }

  constexpr Bytes bytes() const { return size_; }
  constexpr Alignment alignment() const { return alignment_; }

  friend constexpr bool operator==(TypeContour lhs, TypeContour rhs) {
    return lhs.size_ == rhs.size_ and lhs.alignment_ == rhs.alignment_;
  }

  friend constexpr bool operator!=(TypeContour lhs, TypeContour rhs) {
    return not(lhs == rhs);
  }

 private:
  Bytes size_;
  Alignment alignment_;
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_CONTOUR_H
