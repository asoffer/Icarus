#ifndef ICARUS_IR_IR_H
#define ICARUS_IR_IR_H

#include <span>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "ir/emit.h"
#include "parser/parse_tree.h"
#include "type/type.h"

namespace ic {

struct IrContext {
  absl::flat_hash_map<uint32_t, type::Type> identifiers;
  std::vector<type::Type> type_stack;
  EmitContext emit;
};

void ProcessIr(std::span<ParseTree::Node const> nodes, IrContext& context);

inline IrContext ProcessIr(ParseTree const& tree) {
  IrContext context;
  ProcessIr(tree.nodes(), context);
  return context;
}

}  // namespace ic

#endif  // ICARUS_IR_IR_H
