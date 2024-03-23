#ifndef ICARUS_COMMON_INTERNAL_IDENTIFIERS_H
#define ICARUS_COMMON_INTERNAL_IDENTIFIERS_H

#include <string>

#include "nth/container/flyweight_set.h"

namespace ic::internal_common {

nth::flyweight_set<std::string> &Identifiers();

}  // namespace ic::internal_common

#endif  // ICARUS_COMMON_INTERNAL_IDENTIFIERS_H
