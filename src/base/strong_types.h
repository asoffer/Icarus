#ifndef ICARUS_BASE_STRONG_TYPES_H
#define ICARUS_BASE_STRONG_TYPES_H

#include "debug.h"
#include <cstdint>
#include <string>

#define DEFINE_STRONG_INT(type_name, base_type, default_value)                 \
  struct type_name                                                             \
      : public base::StrongIndex<type_name, base_type, default_value> {        \
    type_name(base_type val = default_value)                                   \
        : ::base::StrongIndex<type_name, base_type, default_value>(val) {}     \
  };                                                                           \
  inline std::ostream &operator<<(std::ostream &os, type_name val) {           \
    return os << val.value;                                                    \
  }                                                                            \
  struct type_name

#define DEFINE_STRONG_STRING(type_name)                                        \
  struct type_name : public ::base::StrongString<type_name> {                  \
    explicit type_name(const char *name = "")                                  \
        : ::base::StrongString<type_name>(name) {}                             \
    explicit type_name(std::string name)                                       \
        : ::base::StrongString<type_name>(std::move(name)) {}                  \
  }

namespace base {
template <typename StringType> struct StrongString;

template <typename StringType>
bool operator==(const StrongString<StringType> &lhs,
                const StrongString<StringType> &rhs);
template <typename StringType>
std::ostream &operator<<(std::ostream &os, const StrongString<StringType> &s);

template <typename StringType> struct StrongString {
public:
  explicit StrongString(const char *name = "") : value(name) {}
  explicit StrongString(std::string name) : value(std::move(name)) {}
  const char *c_str() const { return value.c_str(); }
  const std::string& get() const { return value; }
  char operator[](size_t n) const {
    // Because this method is const, it is DEFINED behavior to access a const
    // reference to the character just passed the end of the string. It will
    // return a const reference to '\0'
    ASSERT_LE(n, value.size());
    return value[n];
  }
  size_t size() const { return value.size(); }

  // TODO should this be a StrongString type?
  std::string substr(size_t pos = 0, size_t len = std::string::npos) const {
    return value.substr(pos, len);
  }

private:
  friend struct std::hash<StringType>;
  friend bool operator==<>(const StrongString &lhs, const StrongString &rhs);
  friend std::ostream &operator<<<>(std::ostream &os, const StrongString &s);

  std::string value;
};

template <typename StringType>
inline std::ostream &operator<<(std::ostream &os,
                                const ::base::StrongString<StringType> &s) {
  return os << s.value;
}
template <typename StringType>
inline bool operator==(const StrongString<StringType> &lhs,
                       const StrongString<StringType> &rhs) {
  return lhs.value == rhs.value;
}
template <typename StringType>
inline bool operator!=(const StrongString<StringType> &lhs,
                       const StrongString<StringType> &rhs) {
  return lhs != rhs;
}

template <typename IndexType, typename BaseType, BaseType DefaultValue>
struct StrongIndex {
  explicit StrongIndex(BaseType val = DefaultValue) : value(val) {}
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
