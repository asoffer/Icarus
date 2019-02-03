#ifndef ICARUS_BASE_STRONG_TYPES_H
#define ICARUS_BASE_STRONG_TYPES_H

#include <cstdint>
#include <iosfwd>
#include <string>
#include "debug.h"

#define DEFINE_STRONG_INT(ns, type, underlying_type, default_value)            \
  namespace ns {                                                               \
  struct type {                                                                \
    constexpr type() = default;                                                \
    constexpr explicit type(underlying_type num) : value(num) {}               \
    std::string to_string() const {                                            \
      return #type "." + std::to_string(value);                                \
    }                                                                          \
    bool is_default() const { return value == default_value; }                 \
                                                                               \
    underlying_type value = default_value;                                     \
  };                                                                           \
  inline bool operator==(type lhs, type rhs) {                                 \
    return lhs.value == rhs.value;                                             \
  }                                                                            \
  inline bool operator!=(type lhs, type rhs) { return !(lhs == rhs); }         \
  } /* namespace ns */                                                         \
                                                                               \
  namespace std {                                                              \
  template <>                                                                  \
  struct hash<ns::type> {                                                      \
    decltype(auto) operator()(ns::type val) const noexcept {                   \
      return std::hash<underlying_type>{}(val.value);                          \
    }                                                                          \
  };                                                                           \
  } /* namespace std */                                                        \
  struct FakeStructToAllowSemicolonAfterMacro

#endif  // ICARUS_BASE_STRONG_TYPES_H
