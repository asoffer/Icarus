#ifndef ICARUS_TYPE_DESERIALIZE_H
#define ICARUS_TYPE_DESERIALIZE_H

#include <cstdint>
#include <cstring>

#include "nth/io/serialize/deserialize.h"
#include "nth/io/serialize/reader.h"
#include "nth/meta/concepts.h"
#include "type/parameters.h"
#include "type/type.h"

namespace ic::type {

template <typename, typename T>
struct Indexed {
  void index(T const& t) NTH_ATTRIBUTE(lifetimebound) {
    index_.push_back(std::addressof(t));
  }
  T const& operator[](uint32_t n) { return *index_[n]; }

 private:
  std::vector<T const*> index_;
};

template <nth::io::reader R>
struct TypeDeserializer : R, Indexed<struct ParametersTypeTag, ParametersType> {
  template <typename... Args>
  explicit TypeDeserializer(Args&&... args) : R(std::forward<Args>(args)...) {}

  friend bool NthDeserialize(TypeDeserializer& d, std::integral auto& n) {
    return nth::io::deserialize_fixed(d, n);
  }

  friend bool NthDeserialize(TypeDeserializer& d, Type& t) {
    uint64_t n;
    static_assert(sizeof(n) == sizeof(t));
    if (not nth::io::deserialize_fixed(d, n)) { return false; }
    std::memcpy(&t, &n, sizeof(t));
    return true;
  }

  friend bool NthDeserialize(TypeDeserializer& d, nth::enumeration auto& e) {
    return nth::io::deserialize_fixed(d, e);
  }

  friend bool NthDeserialize(TypeDeserializer& d, ParametersType& p) {
    uint32_t index;
    if (not nth::io::deserialize_fixed(d, index)) { return false; }
    p = static_cast<Indexed<ParametersTypeTag, ParametersType>>(d)[index];
    return true;
  }

  friend bool NthDeserialize(TypeDeserializer& d,
                             ParametersType::Parameter& p) {
    return nth::io::deserialize(d, p.name, p.type);
  }

  template <nth::io::deserializable_with<TypeDeserializer> X,
            nth::io::deserializable_with<TypeDeserializer> Y>
  friend bool NthDeserialize(TypeDeserializer& d, std::pair<X, Y>& pair) {
    return nth::io::deserialize(d, pair.first) and
           nth::io::deserialize(d, pair.second);
  }

  template <nth::io::deserializable_with<TypeDeserializer>... Ts>
  friend bool NthDeserialize(TypeDeserializer& d, std::tuple<Ts...>& tuple) {
    return std::apply(
        [&](auto&... elements) {
          return (nth::io::deserialize(d, elements) and ...);
        },
        tuple);
  }

  template <nth::io::deserializable_with<TypeDeserializer> T>
  friend bool NthDeserialize(TypeDeserializer& d, std::vector<T>& v) {
    bool success = nth::io::deserialize_sequence(d, v);
    if constexpr (nth::type<T> == nth::type<ParametersType>) {
      for (auto const& p : v) { d.index(p); }
    }
    return success;
  }

  friend bool NthDeserialize(TypeDeserializer& d, TypeSystem& ts) {
    TypeSystem scratch;
    if (not(nth::io::deserialize_sequence(d, scratch.parameters) and
            nth::io::deserialize_sequence(d, scratch.returns) and
            nth::io::deserialize_sequence(d, scratch.functions) and
            nth::io::deserialize_sequence(d, scratch.pointee_types) and
            nth::io::deserialize_sequence(d, scratch.buffer_pointee_types) and
            nth::io::deserialize_sequence(d, scratch.slice_element_types))) {
      return false;
    }
    ts.merge_from(scratch);
    return true;
  }
};

}  // namespace ic::type

#endif  // ICARUS_TYPE_DESERIALIZE_H
