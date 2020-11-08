#ifndef ICARUS_BASE_META_H
#define ICARUS_BASE_META_H

#include <concepts>
#include <cstdint>
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

template <typename T>
constexpr bool always_false() {
  return false;
}

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

  friend char const* stringify(MetaValue m) { return m.name(); }

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

  friend char const* stringify(Meta<T>) { return name_; }

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

}  // namespace base

#endif  // ICARUS_BASE_META_H
