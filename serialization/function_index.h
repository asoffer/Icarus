#ifndef ICARUS_SERIALIZATION_FUNCTION_INDEX_H
#define ICARUS_SERIALIZATION_FUNCTION_INDEX_H

#include <cstdint>
#include <ostream>

#include "base/debug.h"
#include "serialization/proto/function_index.pb.h"

namespace serialization {

// An identifier usable to find the byte code for a function within a given
// (implicit module).
struct FunctionIndex {
  // Function indices are represented as a 32-bit unsigned integer. The
  // most-significant bit is reserved to indicate whether the function indexed
  // is a foreign function (defined outside of an Icarus module). This leaves 31
  // bits worth of index space.
  using underlying_type = uint32_t;

  constexpr explicit FunctionIndex() = default;

  explicit FunctionIndex(underlying_type n) : index_(n) {
    ASSERT(n <= MaxLocalIndex);
  }

  static FunctionIndex Foreign(underlying_type n) {
    FunctionIndex l(n);
    l.foreign_ = 1;
    return l;
  }

  friend bool operator==(FunctionIndex, FunctionIndex) = default;
  friend bool operator!=(FunctionIndex, FunctionIndex) = default;

  template <typename H>
  friend H AbslHashValue(H h, FunctionIndex id) {
    return H::combine(std::move(h),
                      (underlying_type{id.foreign_} << IdWidth) | id.index_);
  }

  friend std::ostream &operator<<(std::ostream &os, FunctionIndex l);

  // No module has this identifier.
  static constexpr FunctionIndex Invalid() { return FunctionIndex(); }

  underlying_type value() const { return index_; }
  bool foreign() const { return foreign_; }

  static void Serialize(FunctionIndex from, proto::FunctionIndex &to);
  static bool Deserialize(proto::FunctionIndex from, FunctionIndex &to);

 private:
  static constexpr size_t IdWidth = sizeof(underlying_type) * CHAR_BIT - 1;
  static constexpr size_t MaxLocalIndex = (underlying_type{1} << IdWidth) - 1;

  underlying_type foreign_ : 1     = 0;
  underlying_type index_ : IdWidth = MaxLocalIndex;
};

}  // namespace serialization

#endif  // ICARUS_SERIALIZATION_FUNCTION_INDEX_H
