#ifndef ICARUS_BASE_STRONG_TYPES_H
#define ICARUS_BASE_STRONG_TYPES_H

#include "debug.h"
#include <cstdint>
#include <iosfwd>
#include <string>

#define DEFINE_STRONG_INT(type_name, base_type, default_value)                 \
  struct type_name                                                             \
      : public base::StrongIndex<type_name, base_type, default_value> {        \
    constexpr explicit type_name(base_type val = default_value)                \
        : ::base::StrongIndex<type_name, base_type, default_value>(val) {}     \
  };                                                                           \
  inline std::ostream &operator<<(std::ostream &os, type_name val) {           \
    return os << val.value;                                                    \
  }                                                                            \
  struct type_name

namespace base {
template <typename IndexType, typename BaseType, BaseType DefaultValue>
struct StrongIndex {
  constexpr explicit StrongIndex(BaseType val = DefaultValue) : value(val) {}
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

template <typename IndexType, typename BaseType, BaseType DefaultValue>
inline bool operator<(StrongIndex<IndexType, BaseType, DefaultValue> lhs,
                      StrongIndex<IndexType, BaseType, DefaultValue> rhs) {
  return lhs.value < rhs.value;
}

template <typename IndexType, typename BaseType, BaseType DefaultValue>
inline bool operator<=(StrongIndex<IndexType, BaseType, DefaultValue> lhs,
                      StrongIndex<IndexType, BaseType, DefaultValue> rhs) {
  return !(rhs < lhs);
}

template <typename IndexType, typename BaseType, BaseType DefaultValue>
inline bool operator>(StrongIndex<IndexType, BaseType, DefaultValue> lhs,
                      StrongIndex<IndexType, BaseType, DefaultValue> rhs) {
  return rhs < lhs;
}

template <typename IndexType, typename BaseType, BaseType DefaultValue>
inline bool operator>=(StrongIndex<IndexType, BaseType, DefaultValue> lhs,
                       StrongIndex<IndexType, BaseType, DefaultValue> rhs) {
  return !(lhs < rhs);
}
} // namespace base

namespace std {
template <typename IndexType, typename BaseType, BaseType DefaultValue>
string to_string(base::StrongIndex<IndexType, BaseType, DefaultValue> val) {
  return to_string(val.value);
}
} // namespace std

#define DEFINE_STRONG_HASH(qualified_type)                                     \
  namespace std {                                                              \
  template <> struct hash<qualified_type> {                                    \
    decltype(auto) operator()(qualified_type val) const noexcept {             \
      return std::hash<decltype(val.value)>{}(val.value);                      \
    }                                                                          \
  };                                                                           \
  }                                                                            \
  struct FakeStructToRequireSemicolonInMacro

#endif // ICARUS_BASE_STRONG_TYPES_H
