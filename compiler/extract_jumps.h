#ifndef ICARUS_COMPILER_EXTRACT_JUMPS_H
#define ICARUS_COMPILER_EXTRACT_JUMPS_H

#include <array>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "ast/visitor.h"
#include "base/debug.h"

namespace compiler {

void ExtractJumps(
    absl::flat_hash_map</* to = */ ast::Node const *,
                        /* from = */ std::vector<ast::Node const *>> *map,
    ast::Node const *node);

}  // namespace compiler

#endif  // ICARUS_COMPILER_EXTRACT_JUMPS_H
