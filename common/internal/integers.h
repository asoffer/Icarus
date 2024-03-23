#ifndef ICARUS_COMMON_INTERNAL_INTEGERS_H
#define ICARUS_COMMON_INTERNAL_INTEGERS_H

#include "absl/container/flat_hash_map.h"
#include "common/constant/entry.h"
#include "common/result.h"
#include "nth/container/flyweight_map.h"
#include "nth/io/deserialize/deserialize.h"
#include "nth/numeric/integer.h"

namespace ic::internal_common {

nth::flyweight_map<nth::integer, Constant>& Integers();

}  // namespace ic::internal_common

#endif  // ICARUS_COMMON_INTERNAL_INTEGERS_H
