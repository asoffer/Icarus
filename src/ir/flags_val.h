#ifndef ICARUS_FLAGS_VAL
#define ICARUS_FLAGS_VAL

#include "base/strong_types.h"
DEFINE_STRONG_INT(ir, FlagsVal, size_t, 0);

namespace ir {
bool operator<(FlagsVal lhs, FlagsVal rhs);
bool operator<=(FlagsVal lhs, FlagsVal rhs);
bool operator>(FlagsVal lhs, FlagsVal rhs);
bool operator>=(FlagsVal lhs, FlagsVal rhs);

std::ostream &operator<<(std::ostream &os, FlagsVal f);
}

#endif  // ICARUS_FLAGS_VAL

