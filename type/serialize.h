#ifndef ICARUS_TYPE_SERIALIZE_H
#define ICARUS_TYPE_SERIALIZE_H

#include <cstring>

#include "nth/io/serialize/serialize.h"
#include "nth/io/writer/writer.h"
#include "nth/meta/concepts.h"
#include "type/parameters.h"
#include "type/type.h"

namespace ic::type {

template <nth::io::writer W>
struct TypeSerializer : W {
  template <typename... Args>
  explicit TypeSerializer(Args&&... args) : W(std::forward<Args>(args)...) {}

  friend bool NthSerialize(TypeSerializer& s, std::integral auto n) {
    return nth::io::serialize_fixed(s, n);
  }

  friend bool NthSerialize(TypeSerializer& s, nth::enumeration auto e) {
    return nth::io::serialize_fixed(s, e);
  }

  friend bool NthSerialize(TypeSerializer& s, Type const& t) {
    uint64_t n;
    static_assert(sizeof(n) == sizeof(t));
    std::memcpy(&n, &t, sizeof(n));
    return nth::io::serialize_fixed(s, n);
  }

  friend bool NthSerialize(TypeSerializer& s,
                           ParametersType::Parameter const& p) {
    return nth::io::serialize(s, p.name, p.type);
  }

  template <nth::io::serializable_with<TypeSerializer> X,
            nth::io::serializable_with<TypeSerializer> Y>
  friend bool NthSerialize(TypeSerializer& s, std::pair<X, Y> const& pair) {
    return nth::io::serialize(s, pair.first) and
           nth::io::serialize(s, pair.second);
  }

  template <nth::io::serializable_with<TypeSerializer>... Ts>
  friend bool NthSerialize(TypeSerializer& s, std::tuple<Ts...> const& tuple) {
    return std::apply(
        [&](auto&... elements) {
          return (nth::io::serialize(s, elements) and ...);
        },
        tuple);
  }

  template <nth::io::serializable_with<TypeSerializer> T>
  friend bool NthSerialize(TypeSerializer& s, std::vector<T> const& v) {
    return nth::io::serialize_sequence(s, v);
  }

  friend bool NthSerialize(TypeSerializer& s, TypeSystem const& ts) {
    return nth::io::serialize_sequence(s, ts.parameters) and
           nth::io::serialize_sequence(s, ts.returns) and
           nth::io::serialize_sequence(s, ts.functions) and
           nth::io::serialize_sequence(s, ts.pointee_types) and
           nth::io::serialize_sequence(s, ts.buffer_pointee_types) and
           nth::io::serialize_sequence(s, ts.slice_element_types);
  }
};

}  // namespace ic::type

#endif  // ICARUS_TYPE_SERIALIZE_H
