#ifndef ICARUS_BASE_META_H
#define ICARUS_BASE_META_H

#include <array>
#include <concepts>
#include <cstdint>
#include <ostream>
#include <type_traits>
#include <typeinfo>
#include <utility>

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

template <typename... Ts>
using type_list = void (*)(Ts...);

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

struct MetaValue {
  explicit constexpr MetaValue() = default;

  template <typename H>
  friend H AbslHashValue(H h, MetaValue m) {
    return H::combine(std::move(h), m.value_);
  }

  friend constexpr bool operator==(MetaValue lhs, MetaValue rhs) {
    return lhs.value_ == rhs.value_;
  }

  friend constexpr bool operator!=(MetaValue lhs, MetaValue rhs) {
    return not(lhs == rhs);
  }

  constexpr uintptr_t get() const { return value_; }
  char const* name() const {
    return *reinterpret_cast<char const* const*>(value_);
  }

  template <typename T>
  friend struct Meta;

  friend std::ostream& operator<<(std::ostream& os, MetaValue m) {
    return os << m.name();
  }

 private:
  explicit constexpr MetaValue(uintptr_t val) : value_(val) {}
  uintptr_t value_ = 0;
};

namespace internal_meta {
template <typename, template <typename...> typename>
struct IsAImpl : std::false_type {};
template <typename... TemplateArgs, template <typename...> typename P>
struct IsAImpl<P<TemplateArgs...>, P> : std::true_type {};
}  // namespace internal_meta

template <typename T>
struct Meta {
  using type = T;

  static MetaValue value() {
    return MetaValue{reinterpret_cast<uintptr_t>(&name_)};
  }
  /* implicit */ operator MetaValue() const { return value(); }

  char const* name() const { return name_; }

  friend std::ostream& operator<<(std::ostream& os, Meta) {
    return os << name_;
  }

  template <template <typename...> typename P>
  constexpr bool is_a() const {
    return internal_meta::IsAImpl<T, P>::value;
  }

  template <typename U>
  constexpr bool converts_to() const {
    return std::is_convertible_v<T, U>;
  }

  template <typename U>
  constexpr bool inherits_from() const {
    return std::is_base_of_v<U, T>;
  }

 private:
  static char const* const name_;
};

template <typename T>
char const* const Meta<T>::name_ = typeid(Meta<T>).name();

template <typename T>
Meta<T> meta;

template <typename Lhs, typename Rhs>
constexpr bool operator==(Meta<Lhs>, Meta<Rhs>) {
  return std::is_same_v<Lhs, Rhs>;
}

template <typename Lhs, typename Rhs>
constexpr bool operator!=(Meta<Lhs> lhs, Meta<Rhs> rhs) {
  return not(lhs == rhs);
}

template <typename T>
constexpr bool operator==(Meta<T> lhs, MetaValue rhs) {
  return lhs.value() == rhs;
}

template <typename T>
constexpr bool operator==(MetaValue lhs, Meta<T> rhs) {
  return lhs == rhs.value();
}

template <typename T>
constexpr bool operator!=(Meta<T> lhs, MetaValue rhs) {
  return not(lhs == rhs);
}

template <typename T>
constexpr bool operator!=(MetaValue lhs, Meta<T> rhs) {
  return not(lhs == rhs);
}

template <typename... Ts, typename T>
constexpr bool ContainsImpl(type_list<Ts...>, T*) {
  return ((meta<Ts> == meta<T>) || ...);
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

template <typename T, typename U>
concept different_from = not std::same_as<T, U>;

template <typename T, typename... Ts>
concept one_of = (std::same_as<T, Ts> or ...);

template <typename T, typename... Ts>
concept convertible_to_exactly_one_of = (((std::convertible_to<T, Ts> ? 1 : 0) +
                                          ...) == 1);

template <typename T, typename TypeList>
concept contained_in = Contains<TypeList, T>();

template <typename T>
constexpr bool always_false() {
  return false;
}

template <typename T>
constexpr bool always_false(Meta<T>) {
  return false;
}

template <typename T>
concept Assignable = std::assignable_from<T&, T>;

template <typename T>
concept SatisfiesTupleProtocol = requires(T t) {
  { std::tuple_size<T>::value }
  ->std::integral;
};

template <typename T>
concept Container = requires(T t) {
  typename T::value_type;
  t.begin() == t.end();
  *t.begin();
  ++std::declval<typename T::iterator&>();
};

template <typename T, template <typename> typename Template>
concept is_a = base::meta<T>.template is_a<Template>();

template <typename T>
concept is_enum = std::is_enum_v<T>;

template <typename T>
concept Streamable = requires(T t) {
  { std::declval<std::ostream&>() << t }
  ->std::same_as<std::ostream&>;
};

template <typename From, typename To>
concept PtrConvertibleTo = requires(From* f) {
  { static_cast<To*>(f) }
  ->std::same_as<To*>;
};

template <typename T, typename... Ts>
constexpr ssize_t Index(type_list<Ts...>) {
  ssize_t i = 0;
  (void)((meta<T> == meta<Ts> ? false : (++i, true)) && ...);
  return i == sizeof...(Ts) ? ssize_t{-1} : i;
}

namespace internal_meta {
template <template <typename> typename F, typename TL>
struct array_transform_impl;
template <template <typename> typename F, typename... Ts>
struct array_transform_impl<F, type_list<Ts...>> {
  static constexpr std::array<decltype(F<first<Ts...>>::value), sizeof...(Ts)>
      value{F<Ts>::value...};
};

template <typename>
struct tail_impl;
template <typename T, typename... Ts>
struct tail_impl<type_list<T, Ts...>> {
  using type = type_list<Ts...>;
};

}  // namespace internal_meta

template <template <typename> typename F, typename TL>
inline constexpr auto array_transform =
    internal_meta::array_transform_impl<F, TL>::value;

template <typename TL>
using tail = typename internal_meta::tail_impl<TL>::type;

template <typename H, typename T>
concept Hasher = std::invocable<H, T>and
    std::convertible_to<std::invoke_result_t<H, T>, size_t>;

}  // namespace base

#endif  // ICARUS_BASE_META_H
