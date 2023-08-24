#ifndef ICARUS_IR_EMIT_H
#define ICARUS_IR_EMIT_H

#include <span>

#include "absl/container/flat_hash_map.h"
#include "ir/module.h"
#include "parser/parse_tree.h"
#include "type/type.h"

namespace ic {

struct EmitContext {
  Module module;
  absl::flat_hash_map<ParseTree::Node , type::Type> statement_type;
};

void EmitIr(std::span<ParseTree::Node const> nodes, EmitContext& context);

}  // namespace ic

#endif  // ICARUS_IR_EMIT_H
