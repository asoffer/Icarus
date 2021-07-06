#ifndef ICARUS_BASE_SERIALIZE_H
#define ICARUS_BASE_SERIALIZE_H

#include <iterator>

#include "base/meta.h"
#include "base/raw_iterator.h"

namespace base {
namespace internal_serialize {

template <typename D, typename T>
struct DeserializingIterator {
  using difference_type   = size_t;
  using value_type        = T;
  using pointer           = T const*;
  using reference         = T const&;
  using iterator_category = std::input_iterator_tag;

  DeserializingIterator(D* deserializer, size_t num_elements)
      : deserializer_(deserializer), num_elements_(num_elements) {
    TryIncrement();
  }

  static DeserializingIterator End(D* deserializer) {
    return DeserializingIterator(deserializer, -1);
  }

  static DeserializingIterator FromContainer(D* deserializer);

  reference operator*() const { return value_; }
  pointer operator->() const { return &value_; }

  DeserializingIterator& operator++() {
    TryIncrement();
    return *this;
  }
  void operator++(int) { TryIncrement(); }

  friend bool operator==(DeserializingIterator const& lhs,
                         DeserializingIterator const& rhs) {
    return lhs.deserializer_ == rhs.deserializer_ and
           lhs.num_elements_ == rhs.num_elements_;
  }
  friend bool operator!=(DeserializingIterator const& lhs,
                         DeserializingIterator const& rhs) {
    return not(lhs == rhs);
  }

 private:
  void TryIncrement();

  D* deserializer_;
  ssize_t num_elements_;
  value_type value_;
};

template < typename Seq>
struct TupleElements {};

template <size_t... Ints>
struct TupleElements<std::index_sequence<Ints...>> {
  template <typename D, typename T>
  static void Deserialize(D& d, T& value);

  template <typename S, typename T>
  static void Serialize(S& s, T const& value);

};

template <typename T, typename = void>
struct HasTupleSize : std::false_type {};

template <typename T>
struct HasTupleSize<T, std::void_t<typename std::tuple_size<T>::value_type>>
    : std::true_type {};

}  // namespace internal_serialize

template <typename T>
void Serialize(auto& output, T const& value) {
  if constexpr (requires { BaseSerialize(output, value); }) {
    BaseSerialize(output, value);
  } else if constexpr (std::is_trivially_copyable_v<T>) {
    output.write(RawConstSpanFrom(value));
  } else if constexpr (::base::internal_serialize::HasTupleSize<T>::value) {
    ::base::internal_serialize::TupleElements<
        std::make_index_sequence<std::tuple_size_v<T>>>::Serialize(output, value);

  } else if constexpr (requires {
                         value.begin();
                         value.end();
                         ++value.begin();
                       }) {
    size_t num_elements = std::distance(value.begin(), value.end());
    output.write(RawSpanFrom(num_elements));
    for (auto const& element : value) { Serialize(output, element); }
  } else {
    static_assert(base::always_false<T>());
  }
}

template <typename T>
void Deserialize(auto& input, T& value) {
  using deserializer_type = std::decay_t<decltype(input)>;

  if constexpr (requires { BaseDeserialize(input, value); }) {
    BaseDeserialize(input, value);
  } else if constexpr (std::is_trivially_copyable_v<T>) {
    auto span = input.read(sizeof(value));
    std::memcpy(&value, span.data(), sizeof(value));
  } else if constexpr (::base::internal_serialize::HasTupleSize<T>::value) {
    ::base::internal_serialize::TupleElements<
        std::make_index_sequence<std::tuple_size_v<T>>>::Deserialize(input,
                                                                     value);

  } else if constexpr (
      requires {
        T(std::declval<internal_serialize::DeserializingIterator<
              deserializer_type, typename T::value_type>>(),
          std::declval<internal_serialize::DeserializingIterator<
              deserializer_type, typename T::value_type>>());
      }) {
    using iterator =
        internal_serialize::DeserializingIterator<deserializer_type,
                                                  typename T::value_type>;
    value = T(iterator::FromContainer(&input), iterator::End(&input));
  } else {
    static_assert(base::always_false<T>());
  }
}

template <typename T>
T Deserialize(auto& input) {
  T value;
  Deserialize(input, value);
  return value;
}

namespace internal_serialize {

template <typename D, typename T>
DeserializingIterator<D, T> DeserializingIterator<D, T>::FromContainer(
    D* deserializer) {
  size_t num_elements;
  Deserialize(*deserializer, num_elements);
  return DeserializingIterator(deserializer, num_elements);
}

template <typename D, typename T>
void DeserializingIterator<D, T>::TryIncrement() {
  if (num_elements_ == -1) { return; }
  Deserialize(*deserializer_, value_);
  --num_elements_;
}

template <size_t... Ints>
template <typename D, typename T>
void TupleElements<std::index_sequence<Ints...>>::Deserialize(D& d, T& value) {
  (::base::Deserialize(d, std::get<Ints>(value)), ...);
}

template <size_t... Ints>
template <typename S, typename T>
void TupleElements<std::index_sequence<Ints...>>::Serialize(S& s,
                                                            T const& value) {
  (::base::Serialize(s, std::get<Ints>(value)), ...);
}

}  // namespace internal_serialize
}  // namespace base

#endif  // ICARUS_BASE_SERIALIZE_H
