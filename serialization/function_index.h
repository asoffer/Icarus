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
  using underlying_type = uint32_t;

  constexpr explicit FunctionIndex() = default;

  constexpr explicit FunctionIndex(underlying_type n) : index_(n) {}

  friend bool operator==(FunctionIndex, FunctionIndex) = default;
  friend bool operator!=(FunctionIndex, FunctionIndex) = default;

  template <typename H>
  friend H AbslHashValue(H h, FunctionIndex index) {
    return H::combine(std::move(h), index.value());
  }

  friend std::ostream &operator<<(std::ostream &os, FunctionIndex l);

  // No module has this identifier.
  static constexpr FunctionIndex Invalid() { return FunctionIndex(); }

  underlying_type value() const { return index_; }

  static void Serialize(FunctionIndex from, proto::FunctionIndex &to);
  static bool Deserialize(proto::FunctionIndex from, FunctionIndex &to);

 private:
  underlying_type index_ = std::numeric_limits<underlying_type>::max();
};

}  // namespace serialization

#endif  // ICARUS_SERIALIZATION_FUNCTION_INDEX_H
