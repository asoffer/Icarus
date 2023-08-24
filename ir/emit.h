#ifndef ICARUS_IR_EMIT_H
#define ICARUS_IR_EMIT_H

#include <span>

#include "ir/module.h"
#include "parser/parse_tree.h"

namespace ic {

struct EmitContext {
  Module module;
};

void EmitIr(std::span<ParseTree::Node const> nodes, EmitContext& context);

inline EmitContext EmitIr(ParseTree const& tree) {
  EmitContext context;
  EmitIr(tree.nodes(), context);
  return context;
}

}  // namespace ic

#endif  // ICARUS_IR_EMIT_H
