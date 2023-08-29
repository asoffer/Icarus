#ifndef ICARUS_IR_IR_H
#define ICARUS_IR_IR_H

#include <span>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "diagnostics/consumer/consumer.h"
#include "ir/emit.h"
#include "parser/parse_tree.h"
#include "type/type.h"

namespace ic {

struct IrContext {
  ParseTree::Node const& Node(ParseTree::Node::Index index) {
    return tree[index];
  }

  auto Children(ParseTree::Node::Index index) { return tree.children(index); }

  ParseTree const& tree;
  absl::flat_hash_map<uint32_t, type::Type> identifiers;
  std::vector<type::Type> type_stack;
  std::vector<Token::Kind> operator_stack;
  EmitContext emit;
};

void ProcessIr(std::span<ParseTree::Node const> nodes, IrContext& context,
               diag::DiagnosticConsumer& diag);

inline IrContext ProcessIr(ParseTree const& tree,
                           diag::DiagnosticConsumer& diag) {
  IrContext context{.tree = tree};
  ProcessIr(tree.nodes(), context, diag);
  return context;
}

}  // namespace ic

#endif  // ICARUS_IR_IR_H
