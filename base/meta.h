#ifndef ICARUS_BASE_META_H
#define ICARUS_BASE_META_H

namespace base {

template <typename T>
struct DeductionBlocker {
  using type = T;
};
template <typename T>
using DeductionBlockerT = typename DeductionBlocker<T>::type;

template <typename... Ts>
struct first;

template <typename T, typename... Ts>
struct first<T, Ts...> {
  using type = T;
};

template <typename... Ts>
using first_t = typename first<Ts...>::type;

template <typename T>
struct identity {
  using type = T;
};
template <typename T>
using identity_t = typename identity<T>::type;

}  // namespace base

#endif  // ICARUS_BASE_META_H
