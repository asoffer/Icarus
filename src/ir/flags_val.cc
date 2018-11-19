#include "ir/flags_val.h"
#include "type/flags.h"

namespace ir {
bool operator<(FlagsVal lhs, FlagsVal rhs) {
  return lhs.value != rhs.value && ((lhs.value | rhs.value) == rhs.value);
}

bool operator<=(FlagsVal lhs, FlagsVal rhs) {
  return (lhs.value | rhs.value) == rhs.value;
}

bool operator>(FlagsVal lhs, FlagsVal rhs) {
  return lhs.value != rhs.value && ((lhs.value | rhs.value) == lhs.value);
}

bool operator>=(FlagsVal lhs, FlagsVal rhs) {
  return (lhs.value | rhs.value) == lhs.value;
}

FlagsVal NotFlags(FlagsVal f, type::Flags const *t) {
  return FlagsVal{((static_cast<size_t>(1) << t->members_.size()) - 1) ^
                  f.value};
}

std::ostream &operator<<(std::ostream &os, FlagsVal f) { return os << f.value; }
}  // namespace ir
