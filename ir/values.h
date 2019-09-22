#ifndef ICARUS_VALUES_VAL
#define ICARUS_VALUES_VAL

#include "base/strong_types.h"

namespace ir {

ICARUS_BASE_DEFINE_STRONG_TYPE(EnumVal, size_t{0},  //
                               base::EnableHashing, base::EnableComparisons);
ICARUS_BASE_DEFINE_STRONG_TYPE(FlagsVal, size_t{0},  //
                               base::EnableHashing, base::EnableComparisons);

constexpr FlagsVal operator|(FlagsVal lhs, FlagsVal rhs) {
  return FlagsVal{lhs.value | rhs.value};
}
constexpr FlagsVal operator^(FlagsVal lhs, FlagsVal rhs) {
  return FlagsVal{lhs.value ^ rhs.value};
}
constexpr FlagsVal operator&(FlagsVal lhs, FlagsVal rhs) {
  return FlagsVal{lhs.value & rhs.value};
}

}  // namespace ir

#endif  // ICARUS_VALUES_VAL
