#ifndef ICARUS_BASE_META_H
#define ICARUS_BASE_META_H

#include <array>
#include <concepts>
#include <cstdint>
#include <ostream>
#include <type_traits>
#include <typeinfo>
#include <utility>

#include "nth/meta/type.h"

namespace base {

template <typename... Ts>
using type_list = void (*)(Ts...);

template <typename... Ts>
struct first;

template <typename T, typename... Ts>
struct first<T, Ts...> {
  using type = T;
};

template <typename... Ts>
using first_t = typename first<Ts...>::type;

namespace internal {

template <typename T, typename U>
struct type_list_pair_catter;
template <typename... P1s, typename... P2s>
struct type_list_pair_catter<type_list<P1s...>, type_list<P2s...>> {
  using type = type_list<P1s..., P2s...>;
};

template <typename... Ts>
struct type_list_catter {
  using type = type_list<>;
};
template <typename T, typename... Ts>
struct type_list_catter<T, Ts...>
    : type_list_pair_catter<T, typename type_list_catter<Ts...>::type> {};

}  // namespace internal

template <typename... TLs>
using type_list_cat = typename ::base::internal::type_list_catter<TLs...>::type;

template <typename... Ts, typename T>
constexpr bool ContainsImpl(type_list<Ts...>, T*) {
  return ((nth::type<Ts> == nth::type<T>) || ...);
}

template <typename TL, typename T>
constexpr bool Contains() {
  return ContainsImpl(TL{}, static_cast<T*>(nullptr));
}

template <typename... Ts>
constexpr auto Length(type_list<Ts...>) {
  return sizeof...(Ts);
}
constexpr void Length(void*) {}

template <typename T, typename... Ts>
concept one_of = (std::same_as<T, Ts> or ...);

template <typename T>
constexpr bool always_false() {
  return false;
}

constexpr bool always_false(nth::Type auto) { return false; }

template <typename T>
concept SatisfiesTupleProtocol =
    std::integral<std::decay_t<decltype(std::tuple_size<T>::value)>>;

template <typename T>
concept Container = requires(T t) {
  typename T::value_type;
  t.begin() == t.end();
  *t.begin();
  ++std::declval<typename T::iterator&>();
};

template <typename T>
concept is_enum = std::is_enum_v<T>;

template <typename T>
concept Streamable = requires(T t) {
  { std::declval<std::ostream&>() << t } -> std::same_as<std::ostream&>;
};

template <typename From, typename To>
concept PtrConvertibleTo = requires(From* f) {
  { static_cast<To*>(f) } -> std::same_as<To*>;
};

template <typename T, typename... Ts>
constexpr ssize_t Index(type_list<Ts...>) {
  ssize_t i = 0;
  (void)((nth::type<T> == nth::type<Ts> ? false : (++i, true)) && ...);
  return i == sizeof...(Ts) ? ssize_t{-1} : i;
}

namespace internal_nth::type {

template <typename>
struct tail_impl;
template <typename T, typename... Ts>
struct tail_impl<type_list<T, Ts...>> {
  using type = type_list<Ts...>;
};

template <typename, template <typename> typename>
struct all_of_impl;

template <typename... Ts, template <typename> typename Predicate>
struct all_of_impl<type_list<Ts...>, Predicate> {
  static constexpr bool value = (Predicate<Ts>::value and ...);
};

template <typename T, typename TL, template <typename> typename Predicate,
          bool = Predicate<T>::value>
struct PrependIf;
template <typename T, typename... Ts, template <typename> typename Predicate>
struct PrependIf<T, type_list<Ts...>, Predicate, true> {
  using type = type_list<T, Ts...>;
};
template <typename T, typename TL, template <typename> typename Predicate>
struct PrependIf<T, TL, Predicate, false> {
  using type = TL;
};

template <typename, template <typename> typename>
struct filter_impl;

template <template <typename> typename Predicate>
struct filter_impl<type_list<>, Predicate> {
  using type = type_list<>;
};

template <typename T, typename... Ts, template <typename> typename Predicate>
struct filter_impl<type_list<T, Ts...>, Predicate>
    : PrependIf<T, typename filter_impl<type_list<Ts...>, Predicate>::type,
                Predicate> {};

}  // namespace internal_nth::type

template <typename TL, template <typename> typename Predicate>
inline constexpr bool all_of = internal_nth::type::all_of_impl<TL, Predicate>::value;

template <typename TL, template <typename> typename Predicate>
using filter = typename internal_nth::type::filter_impl<TL, Predicate>::type;

template <typename TL>
using tail = typename internal_nth::type::tail_impl<TL>::type;

template <typename H, typename T>
concept Hasher = std::invocable<H, T> and
    std::convertible_to<std::invoke_result_t<H, T>, size_t>;

}  // namespace base

#endif  // ICARUS_BASE_nth::type_H
