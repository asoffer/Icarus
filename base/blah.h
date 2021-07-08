namespace internal_serialize {

// template <typename D, typename T>
// struct DeserializingIterator {
//   using difference_type   = size_t;
//   using value_type        = T;
//   using pointer           = T const*;
//   using reference         = T const&;
//   using iterator_category = std::input_iterator_tag;
// 
//   DeserializingIterator(D* deserializer, size_t num_elements)
//       : deserializer_(deserializer), num_elements_(num_elements) {
//     TryIncrement();
//   }
// 
//   static DeserializingIterator End(D* deserializer) {
//     return DeserializingIterator(deserializer, -1);
//   }
// 
//   static DeserializingIterator FromContainer(D* deserializer);
// 
//   reference operator*() const { return *value_; }
//   pointer operator->() const { return &*value_; }
// 
//   DeserializingIterator& operator++() {
//     TryIncrement();
//     return *this;
//   }
//   void operator++(int) { TryIncrement(); }
// 
//   friend bool operator==(DeserializingIterator const& lhs,
//                          DeserializingIterator const& rhs) {
//     return lhs.deserializer_ == rhs.deserializer_ and
//            lhs.num_elements_ == rhs.num_elements_;
//   }
//   friend bool operator!=(DeserializingIterator const& lhs,
//                          DeserializingIterator const& rhs) {
//     return not(lhs == rhs);
//   }
// 
//  private:
//   void TryIncrement();
// 
//   D* deserializer_;
//   ssize_t num_elements_;
//   std::optional<value_type> value_;
// };
// 
// template <typename T>
// void SerializeOne(auto& output, T const& value) {
//   } else if constexpr (requires {
//                          value.begin();
//                          value.end();
//                          ++value.begin();
//                        }) {
//     size_t num_elements = std::distance(value.begin(), value.end());
//     LOG("", "%s -> %d", typeid(T).name(), num_elements);
//     ::base::Serialize(output, num_elements);
//     for (auto const& element : value) {
//       LOG("", "  %s", typeid(element).name());
//       Serialize(output, element);
//     }
//   } else {
//     static_assert(base::always_false<T>());
//   }
// }
// 
// template <::base::Assignable T>
// void DeserializeOne(auto& input, T& value) {
//   } else if constexpr (
//       requires {
//         T(std::declval<internal_serialize::DeserializingIterator<
//               deserializer_type, typename T::value_type>>(),
//           std::declval<internal_serialize::DeserializingIterator<
//               deserializer_type, typename T::value_type>>());
//       }) {
//     using iterator =
//         internal_serialize::DeserializingIterator<deserializer_type,
//                                                   typename T::value_type>;
//     value = T(iterator::FromContainer(&input), iterator::End(&input));
//   } else {
//     static_assert(base::always_false<T>());
//   }
// }
// 
// 
}  // namespace internal_serialize

// template <typename T>
// T Deserialize(auto& input) {
//   return ::base::internal_serialize::DeserializeOne<T>(input);
// }

namespace internal_serialize {

template <typename D, typename T>
DeserializingIterator<D, T> DeserializingIterator<D, T>::FromContainer(
    D* deserializer) {
  size_t num_elements;
  ::base::Deserialize(*deserializer, num_elements);
  LOG("", "%s <- %d", typeid(T).name(), num_elements);
  return DeserializingIterator(deserializer, num_elements);
}

template <typename D, typename T>
void DeserializingIterator<D, T>::TryIncrement() {
  if (num_elements_ == -1) { return; }
  value_.reset();
  value_.emplace(::base::Deserialize<T>(*deserializer_));
  --num_elements_;
}


}  // namespace internal_serialize

