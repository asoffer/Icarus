#ifndef ICARUS_FLAGS_VAL
#define ICARUS_FLAGS_VAL

#include "base/strong_types.h"

namespace type {
struct Flags;
}  // namespace type

DEFINE_STRONG_INT(ir, FlagsVal, size_t, 0);

namespace ir {
bool operator<(FlagsVal lhs, FlagsVal rhs);
bool operator<=(FlagsVal lhs, FlagsVal rhs);
bool operator>(FlagsVal lhs, FlagsVal rhs);
bool operator>=(FlagsVal lhs, FlagsVal rhs);

inline FlagsVal operator|(FlagsVal lhs, FlagsVal rhs) {
  return FlagsVal{lhs.value | rhs.value};
}
inline FlagsVal operator^(FlagsVal lhs, FlagsVal rhs) {
  return FlagsVal{lhs.value ^ rhs.value};
}
inline FlagsVal operator&(FlagsVal lhs, FlagsVal rhs) {
  return FlagsVal{lhs.value & rhs.value};
}

FlagsVal NotFlags(FlagsVal f, type::Flags const *t);

std::ostream &operator<<(std::ostream &os, FlagsVal f);

}  // namespace ir

#endif  // ICARUS_FLAGS_VAL

