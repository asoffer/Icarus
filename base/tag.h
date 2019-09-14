
namespace base {
template <typename T>
struct Tag {
  using type = T;
};

template <typename TagType, typename T>
struct Tagged : public T {
  Tagged(T t) : T(std::move(t)) {}
  using tag_type  = TagType;
  using base_type = T;
};

template <typename T>
struct IsTagged : public std::false_type {};

template <typename TagType, typename T>
struct IsTagged<Tagged<TagType, T>> : public std::true_type {
  using tag_type  = TagType;
  using base_type = T;
};

template <typename T>
constexpr bool IsTaggedV = IsTagged<T>::value;

}  // namespace base
