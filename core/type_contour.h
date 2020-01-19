#ifndef ICARUS_CORE_TYPE_CONTOUR_H
#define ICARUS_CORE_TYPE_CONTOUR_H

#include "core/bytes.h"
#include "core/alignment.h"

namespace core {

// `TypeContour` represents the binary-level requirements for a type, including
// its size in bytes, and required alignment.
struct TypeContour {
  constexpr explicit TypeContour() : size_(0), alignment_(1) {}

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
  constexpr TypeContour(Bytes size, Alignment alignment)
      : size_(size), alignment_(alignment) {}
  Bytes size_;
  Alignment alignment_;
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_CONTOUR_H
