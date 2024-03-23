#ifndef ICARUS_COMMON_INTERNAL_STRINGS_H
#define ICARUS_COMMON_INTERNAL_STRINGS_H

#include <string>

#include "absl/container/flat_hash_map.h"
#include "common/constant/entry.h"
#include "common/result.h"
#include "common/to_bytes.h"
#include "nth/container/flyweight_map.h"
#include "nth/io/deserialize/deserialize.h"

namespace ic::internal_common {

nth::flyweight_map<std::string, Constant>& Strings();

}  // namespace ic::internal_common

#endif  // ICARUS_COMMON_INTERNAL_STRINGS_H
