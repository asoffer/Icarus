#ifndef ICARUS_BASE_META_H
#define ICARUS_BASE_META_H

#include "nth/meta/sequence.h"
#include "nth/meta/type.h"

namespace base {

template <typename... Ts>
using type_list = void (*)(Ts...);

template <typename... Ts>
constexpr auto ToSeq(type_list<Ts...>) {
  return nth::type_sequence<Ts...>;
}

template <nth::Sequence auto seq>
using FromSeq = decltype(seq.reduce([](auto... ts) {
  void (*p)(nth::type_t<ts>...) = nullptr;
  return p;
}));

template <typename T>
constexpr bool always_false() {
  return false;
}

constexpr bool always_false(nth::Type auto) { return false; }

}  // namespace base

#endif  // ICARUS_BASE_nth::type_H
