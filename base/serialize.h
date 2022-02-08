#ifndef ICARUS_BASE_SERIALIZE_H
#define ICARUS_BASE_SERIALIZE_H

#include <concepts>
#include <iterator>
#include <optional>

#include "absl/types/span.h"
#include "base/debug.h"
#include "base/meta.h"
#include "base/raw_iterator.h"

namespace base {

template <typename S>
concept Serializer = requires(S s) {
  { (void)s.write_bytes(std::declval<absl::Span<std::byte const>>()) }
  ->std::same_as<void>;
};

template <typename D>
concept Deserializer = requires(D d) {
  { d.read_bytes(std::declval<size_t>()) }
  ->std::convertible_to<absl::Span<std::byte const>>;
};


// Primary entry-points to this library. `Serialize` accepts an object `s`
// satisfying the `base::Serializer` concept, along with any number of values
// which can be serialized, and will serialize them with `s` in the order
// provided.
//
// `base::Deserialize` accepts an object satisfying the `base::Deserializer`
// concept and any number of objects satisfying the `base::Assignable` concept
// (can be assigned to from itself). The deserializer is read from, assigning to
// each of the `values...` in order.
void Serialize(base::Serializer auto& s, auto const&... values);
bool Deserialize(base::Deserializer auto& d, base::Assignable auto&... values);

template <typename T, typename S>
concept SerializableBy = Serializer<S>and requires(T t, S s) {
  base::Serialize(s, t);
};

namespace internal_serialize {

template <typename T, typename = void>
struct AssignableTypeImpl {};

template <typename T>
struct AssignableTypeImpl<T, std::enable_if_t<std::is_const_v<T>>>
    : AssignableTypeImpl<std::remove_const_t<T>> {};

template <typename T>
struct AssignableTypeImpl<
    T, std::enable_if_t<not std::is_const_v<T> and base::Assignable<T>>> {
  using type = T;
};

template <typename, typename>
struct Tuplify {};

template <typename T, int... Ints>
struct Tuplify<T, std::index_sequence<Ints...>> {
  using type = std::tuple<
      typename AssignableTypeImpl<std::tuple_element_t<Ints, T>>::type...>;
};

template <typename T>
struct AssignableTypeImpl<
    T, std::enable_if_t<not std::is_const_v<T> and not base::Assignable<T> and
                        base::SatisfiesTupleProtocol<T>>>
    : Tuplify<T, std::make_index_sequence<std::tuple_size_v<T>>> {};
template <typename T>
struct AssignableTypeImpl<
    T, std::enable_if_t<not std::is_const_v<T> and not base::Assignable<T> and
                        not base::SatisfiesTupleProtocol<T> and
                        std::is_trivially_copyable_v<T>>> {
  using type = T;
};

template <typename T>
using AssignableType = typename AssignableTypeImpl<T>::type;

template <base::Deserializer D, typename T>
struct DeserializingIterator {
  using difference_type   = size_t;
  using value_type        = T;
  using pointer           = T const*;
  using reference         = T const&;
  using iterator_category = std::input_iterator_tag;

  DeserializingIterator(D* deserializer, size_t num_elements, bool* ok)
      : deserializer_(deserializer), num_elements_(num_elements), ok_(ok) {}

  static DeserializingIterator FromContainer(D* deserializer, bool* ok) {
    size_t num_elements;
    *ok = base::Deserialize(*deserializer, num_elements);
    if (not *ok) { return End(deserializer); }
    return DeserializingIterator(deserializer, num_elements, ok);
  }

  static DeserializingIterator End(D* deserializer) {
    return DeserializingIterator(deserializer, 0, nullptr);
  }

  reference operator*() { return *get(); }
  pointer operator->() { return get(); }

  DeserializingIterator& operator++() {
    get();
    ClearValue();
    --num_elements_;
    return *this;
  }
  void operator++(int) {
    get();
    ClearValue();
    --num_elements_;
  }

  friend bool operator==(DeserializingIterator const& lhs,
                         DeserializingIterator const& rhs) {
    ASSERT(lhs.deserializer_ == rhs.deserializer_);
    return lhs.num_elements_ == rhs.num_elements_;
  }
  friend bool operator!=(DeserializingIterator const& lhs,
                         DeserializingIterator const& rhs) {
    return not(lhs == rhs);
  }

 private:
  void ReadValue() {
    ASSERT(value_.has_value() == false);
    if constexpr (base::Assignable<value_type>) {
      value_.emplace();
      ok_ = base::Deserialize(*deserializer_, *value_);
    } else {
      using type = AssignableType<value_type>;
      type value;
      ok_ = base::Deserialize(*deserializer_, value);
      value_.reset();
      value_.emplace(std::make_from_tuple<value_type>(std::move(value)));
    }
    if (not ok_) { num_elements_ = 0; }
  }

  void ClearValue() {
    ASSERT(value_.has_value() == true);
    value_.reset();
  }

  pointer get() {
    if (not value_.has_value()) { ReadValue(); }
    return &*value_;
  }

  D* deserializer_;
  ssize_t num_elements_;
  std::optional<value_type> value_;
  bool ok_;
};

template <typename value_type>
void SerializeOne(base::Serializer auto& s, value_type const& value) {
  if constexpr (requires { s.write(std::declval<value_type>()); }) {
    s.write(value);
  } else if constexpr (requires { BaseSerialize(s, value); }) {
    BaseSerialize(s, value);
  } else if constexpr (base::SatisfiesTupleProtocol<value_type>) {
    std::apply([&](auto&... values) { base::Serialize(s, values...); }, value);
  } else if constexpr (requires {
                         value.begin();
                         value.end();
                         ++std::declval<typename std::decay_t<decltype(
                             value)>::const_iterator&>();
                       }) {
    size_t num_elements = std::distance(value.begin(), value.end());
    base::Serialize(s, num_elements);
    for (auto const& element : value) { Serialize(s, element); }
  } else {
    static_assert(base::always_false<value_type>());
  }
}

template <base::Assignable value_type>
bool DeserializeOne(base::Deserializer auto& d, value_type& value) {
  using deserializer_type = std::decay_t<decltype(d)>;

  if constexpr (requires { d.read(std::declval<value_type&>()); }) {
    return d.read(value);

  } else if constexpr (requires {
    { BaseDeserialize(d, value) } -> std::convertible_to<bool>; }) {
    return BaseDeserialize(d, value);

  } else if constexpr (base::SatisfiesTupleProtocol<value_type>) {
    return std::apply(
        [&](base::Assignable auto&... values) {
          return base::Deserialize(d, values...);
        },
        value);

  } else if constexpr (
      requires {
        value_type(std::declval<internal_serialize::DeserializingIterator<
                       deserializer_type, typename value_type::value_type>>(),
                   std::declval<internal_serialize::DeserializingIterator<
                       deserializer_type, typename value_type::value_type>>());
      }) {
    bool ok        = true;
    using iterator = internal_serialize::DeserializingIterator<
        deserializer_type, typename value_type::value_type>;
    value = value_type(iterator::FromContainer(&d, &ok), iterator::End(&d));
    return ok;

  } else {
    static_assert(base::always_false<value_type>());
  }
}

}  // namespace internal_serialize

void Serialize(base::Serializer auto& s, auto const&... values) {
  (base::internal_serialize::SerializeOne(s, values), ...);
}

bool Deserialize(base::Deserializer auto& d, base::Assignable auto&... values) {
  return (base::internal_serialize::DeserializeOne(d, values) and ...);
}

template <base::Assignable value_type>
value_type Deserialize(base::Deserializer auto& d) {
  value_type value;
  bool ok = base::internal_serialize::DeserializeOne(d, value);
  ASSERT(ok == true);
  return value;
}

}  // namespace base

#endif  // ICARUS_BASE_SERIALIZE_H
