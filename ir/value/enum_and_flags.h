#ifndef ICARUS_VALUE_ENUM_AND_FLAGS_H
#define ICARUS_VALUE_ENUM_AND_FLAGS_H

#include "base/strong_types.h"

namespace ir {

ICARUS_BASE_DEFINE_STRONG_TYPE(EnumVal, uint64_t{0}, base::EnableHashing);
ICARUS_BASE_DEFINE_STRONG_TYPE(FlagsVal, uint64_t{0}, base::EnableHashing);

constexpr FlagsVal operator|(FlagsVal lhs, FlagsVal rhs) {
  return FlagsVal{lhs.value | rhs.value};
}
constexpr FlagsVal operator^(FlagsVal lhs, FlagsVal rhs) {
  return FlagsVal{lhs.value ^ rhs.value};
}
constexpr FlagsVal operator&(FlagsVal lhs, FlagsVal rhs) {
  return FlagsVal{lhs.value & rhs.value};
}
constexpr bool operator<=(FlagsVal lhs, FlagsVal rhs) {
  return (lhs | rhs) == rhs;
}
constexpr bool operator>=(FlagsVal lhs, FlagsVal rhs) { return rhs <= lhs; }
constexpr bool operator<(FlagsVal lhs, FlagsVal rhs) {
  return lhs <= rhs and lhs != rhs;
}
constexpr bool operator>(FlagsVal lhs, FlagsVal rhs) { return rhs < lhs; }
}  // namespace ir

#endif  // ICARUS_VALUE_ENUM_AND_FLAGS_H
