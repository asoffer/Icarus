#ifndef ICARUS_BASE_VARIANT_H
#define ICARUS_BASE_VARIANT_H

#include "types.h"
namespace base {
template <typename T> void Destruct(void *ptr) {
  reinterpret_cast<T *>(ptr)->~T();
}
template <typename T> void Copy(const void *from, void *to) {
  new (reinterpret_cast<T *>(to)) T(*reinterpret_cast<const T *>(from));
}

template <typename T> void Move(void *from, void *to) {
  new (reinterpret_cast<T *>(to)) T(std::move(*reinterpret_cast<T *>(from)));
}


inline constexpr size_t max(size_t lhs, size_t rhs) {
  return lhs < rhs ? rhs : lhs;
}

template <size_t... Ns> struct Max {};
template <size_t N, size_t... Ns>
struct Max<N, Ns...>
    : public std::integral_constant<size_t, max(N, Max<Ns...>::value)> {};
template <size_t N> struct Max<N> : public std::integral_constant<size_t, N> {};

template <typename Needle, typename... Haystack> struct IndexOf {};
template <typename Needle>
struct IndexOf<Needle> : public std::integral_constant<size_t, 1> {};

template <typename Needle, typename FirstHaystack, typename... RestHaystack>
struct IndexOf<Needle, FirstHaystack, RestHaystack...>
    : public std::integral_constant<
          size_t, std::is_same<Needle, FirstHaystack>::value
                      ? 0
                      : 1 + IndexOf<Needle, RestHaystack...>::value> {};

template <typename... Ts> struct alignas(Ts...) variant {
public:
  variant() = delete;

  variant(const variant &v) : kind_(v.kind_) {
    copy_[v.kind_](&v.data_, &data_);
  }
  variant(variant &&v) : kind_(v.kind_) { move_[v.kind_](&v.data_, &data_); }

  ~variant() { destroy_[kind_](&data_); }

  template <typename T> variant(const T &val) {
    using BasicT            = typename std::remove_reference<T>::type;
    constexpr u8 type_index = IndexOf<BasicT, Ts...>::value;
    static_assert(type_index < sizeof...(Ts), "");
    kind_ = type_index;
    new (&data_) BasicT(val);
  }

  template <typename T> variant(T &&val) {
    using BasicT            = typename std::remove_reference<T>::type;
    constexpr u8 type_index = IndexOf<BasicT, Ts...>::value;
    static_assert(type_index < sizeof...(Ts), "");
    kind_ = type_index;
    new (&data_) BasicT(std::move(val));
  }

  template <typename T> void operator=(T &&val) {
    using BasicT            = typename std::remove_reference<T>::type;
    constexpr u8 type_index = IndexOf<BasicT, Ts...>::value;
    static_assert(type_index < sizeof...(Ts), "");

    destroy_[kind_](&data_);
    kind_ = type_index;
    new (&data_) BasicT(std::move(val));
  }

  // TODO if types match avoid the destruction.
  void operator=(const variant &val) {
    destroy_[kind_](&data_);
    kind_ = val.kind_;
    copy_[kind_](&val.data_, &data_);
  }

  void operator=(variant &&val) {
    destroy_[kind_](&data_);
    kind_ = val.kind_;
    move_[kind_](&val.data_, &data_);
  }

  template <typename T> void operator=(const T &val) {
    using BasicT            = typename std::remove_reference<T>::type;
    constexpr u8 type_index = IndexOf<BasicT, Ts...>::value;
    static_assert(type_index < sizeof...(Ts), "");

    destroy_[kind_](&data_);
    kind_ = type_index;
    new (&data_) BasicT(val);
  }

  template <typename T> const T &as() const {
    constexpr u8 type_index = IndexOf<T, Ts...>::value;
    ASSERT_EQ(type_index, kind_);

    static_assert(type_index < sizeof...(Ts), "");
    return *reinterpret_cast<const T *>(&data_);
  }

  template <typename T> T &as() {
    constexpr u8 type_index = IndexOf<T, Ts...>::value;
    static_assert(type_index < sizeof...(Ts), "");
    ASSERT_EQ(type_index, kind_);
    return *reinterpret_cast<T *>(&data_);
  }

private:
  using Storage =
      typename std::aligned_storage<Max<sizeof(Ts)...>::value,
                                    Max<alignof(Ts)...>::value>::type;
  Storage data_;
  u8 kind_ = 0;

  static constexpr std::array<void (*)(void *), sizeof...(Ts)> destroy_{
      {Destruct<Ts>...}};
  static constexpr std::array<void (*)(const void *, void *), sizeof...(Ts)>
      copy_{{Copy<Ts>...}};
  static constexpr std::array<void (*)(void *, void *), sizeof...(Ts)> move_{
      {Move<Ts>...}};
};

template <typename... Ts>
constexpr std::array<void (*)(void *), sizeof...(Ts)> variant<Ts...>::destroy_;

template <typename... Ts>
constexpr std::array<void (*)(const void *, void *), sizeof...(Ts)>
    variant<Ts...>::copy_;

template <typename... Ts>
constexpr std::array<void (*)(void *, void *), sizeof...(Ts)>
    variant<Ts...>::move_;

} // namespace base
#endif // ICARUS_BASE_VARIANT_H
