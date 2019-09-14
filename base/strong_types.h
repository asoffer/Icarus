#ifndef ICARUS_BASE_STRONG_TYPES_H
#define ICARUS_BASE_STRONG_TYPES_H

#include <string>
#include "absl/strings/str_cat.h"
#include "base/stringify.h"

namespace base  {
template <typename Tag, typename UnderlyingType>
struct Strong {
  constexpr Strong() = default;
  constexpr explicit Strong(UnderlyingType num) : value(num) {}
  std::string to_string() const {
    using base::stringify;
    return stringify(value);
  }

  template <typename H>
  friend H AbslHashValue(H h, Strong v) {
    return H::combine(std::move(h), v.value);
  }

  friend constexpr bool operator==(Strong lhs, Strong rhs) {
    return lhs.value == rhs.value;
  }

  friend constexpr bool operator!=(Strong lhs, Strong rhs) {
    return !(lhs == rhs);
  }

  UnderlyingType value{};
};
}  // namespace base

#define ICARUS_BASE_DEFINE_STRONG_TYPE(name, default_value)                    \
  struct name : public base::Strong<name, decltype(default_value)> {           \
    explicit constexpr name(decltype(default_value) val = default_value)       \
        : base::Strong<name, decltype(default_value)>(val) {}                  \
    friend std::string stringify(name val) {                                   \
      return absl::StrCat(#name, "(", val.value, ")");                         \
    }                                                                          \
  }
#endif  // ICARUS_BASE_STRONG_UnderlyingTypeS_H
