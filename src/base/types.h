#ifndef ICARUS_BASE_TYPES_H
#define ICARUS_BASE_TYPES_H

#include <cstdint>
#include <string>

#define DEFINE_TYPE(new_name, old_name, size)                                  \
  using new_name = old_name;                                                   \
  static_assert(sizeof(new_name) == size, "")

DEFINE_TYPE(i8, int8_t, 1);
DEFINE_TYPE(i16, int16_t, 2);
DEFINE_TYPE(i32, int32_t, 4);
DEFINE_TYPE(i64, int64_t, 8);

DEFINE_TYPE(u8, uint8_t, 1);
DEFINE_TYPE(u16, uint16_t, 2);
DEFINE_TYPE(u32, uint32_t, 4);
DEFINE_TYPE(u64, uint64_t, 8);
#undef DEFINE_TYPE

#define DEFINE_STRONG_INT(type_name, base_type, default_value)                 \
  struct type_name                                                             \
      : public base::StrongIndex<type_name, base_type, default_value> {        \
    type_name(base_type val = default_value)                                   \
        : base::StrongIndex<type_name, base_type, default_value>(val) {}       \
  };                                                                           \
  inline std::ostream &operator<<(std::ostream &os, type_name val) {           \
    return os << val.value;                                                    \
  }                                                                            \
  struct type_name

namespace base {
template <typename IndexType, typename BaseType, BaseType DefaultValue>
struct StrongIndex {
  StrongIndex(BaseType val = DefaultValue) : value(val) {}
  bool is_default() const { return value == DefaultValue; }

  BaseType value;
};

template <typename IndexType, typename BaseType, BaseType DefaultValue>
inline bool operator==(StrongIndex<IndexType, BaseType, DefaultValue> lhs,
                       StrongIndex<IndexType, BaseType, DefaultValue> rhs) {
  return lhs.value == rhs.value;
}

template <typename IndexType, typename BaseType, BaseType DefaultValue>
inline bool operator!=(StrongIndex<IndexType, BaseType, DefaultValue> lhs,
                       StrongIndex<IndexType, BaseType, DefaultValue> rhs) {
  return !(lhs == rhs);
}
} // namespace base

namespace std {
template <typename IndexType, typename BaseType, BaseType DefaultValue>
string to_string(base::StrongIndex<IndexType, BaseType, DefaultValue> val) {
  return to_string(val.value);
}
} // namespace std

#define DEFINE_STRONG_INT_HASH(qualified_type)                                 \
  namespace std {                                                              \
  template <> struct hash<qualified_type> {                                    \
    decltype(auto) operator()(qualified_type val) const noexcept {             \
      return std::hash<decltype(val.value)>{}(val.value);                      \
    }                                                                          \
  };                                                                           \
  }                                                                            \
  struct FakeStructToRequireSemicolonInMacro

#endif // ICARUS_BASE_TYPES_H
