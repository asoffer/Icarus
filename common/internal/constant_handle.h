#ifndef ICARUS_COMMON_INTERNAL_CONSTANT_HANDLE
#define ICARUS_COMMON_INTERNAL_CONSTANT_HANDLE

#include <limits>
#include <utility>

#include "common/constant/category.h"
#include "common/result.h"
#include "common/strong_identifier_type.h"
#include "nth/io/deserialize/deserialize.h"
#include "nth/io/serialize/serialize.h"

namespace ic::internal_constants {

struct from_representation_t {};

template <typename T>
struct ConstantHandle : protected StrongIdentifierType<T, uint32_t> {
  explicit constexpr ConstantHandle(
      uint32_t n = std::numeric_limits<uint32_t>::max())
      : StrongIdentifierType<T, uint32_t>(n) {
    static_assert(sizeof(T) == sizeof(uint32_t));
  }

  operator T const&() const& { return static_cast<T&>(*this); }
  operator T&() & { return static_cast<T&>(*this); }

  friend bool operator==(ConstantHandle lhs, ConstantHandle rhs) {
    return lhs.value() == rhs.value();
  }

  friend bool operator!=(ConstantHandle lhs, ConstantHandle rhs) {
    return lhs.value() != rhs.value();
  }

  static T FromRepresentation(uint32_t n) { return T(from_representation_t{}, n); }
  static uint32_t ToRepresentation(T n) { return n.value(); }

  template <typename H>
  friend H AbslHashValue(H h, T t) {
    return H::combine(std::move(h), t.value());
  }

  friend void NthPrint(auto& p, auto& f, ConstantHandle t) requires requires {
    typename T::backing_type;
  }
  { f(p, static_cast<typename T::backing_type const&>(static_cast<T>(t))); }

  friend Result NthSerialize(auto& s, ConstantHandle t) {
    return nth::io::write_integer(s, t.value());
  }

  friend Result NthDeserialize(auto& d, T& t) {
    return nth::io::read_integer(d, t.mutable_value());
  }
};

}  // namespace ic::internal_constants

#endif  // ICARUS_COMMON_INTERNAL_CONSTANT_HANDLE
