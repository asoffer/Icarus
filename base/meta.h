#ifndef ICARUS_BASE_META_H
#define ICARUS_BASE_META_H

namespace base {

template <typename... Ts>
struct first;

template <typename T, typename... Ts>
struct first<T, Ts...> {
  using type = T;
};

template <typename... Ts>
using first_t = typename first<Ts...>::type;

}  // namespace base

#endif // ICARUS_BASE_META_H
