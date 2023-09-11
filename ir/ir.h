#ifndef ICARUS_IR_IR_H
#define ICARUS_IR_IR_H

#include <optional>
#include <span>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/inlined_vector.h"
#include "diagnostics/consumer/consumer.h"
#include "ir/emit.h"
#include "ir/module.h"
#include "ir/module_id.h"
#include "jasmin/value.h"
#include "nth/debug/debug.h"
#include "parser/parse_tree.h"
#include "type/type.h"

namespace ic {

struct IrContext {
  ParseTree::Node const& Node(ParseTree::Node::Index index) {
    return tree[index];
  }

  auto Children(ParseTree::Node::Index index) { return tree.children(index); }

  Module const& module(ModuleId id) const { return modules[id.value()]; }

  void ProcessIr(diag::DiagnosticConsumer& diag);

  template <typename T>
  std::optional<T> EvaluateAs(ParseTree::Node::Index subtree) const {
    return T{};
  }

  ParseTree const& tree;
  absl::flat_hash_map<uint32_t, type::Type> identifiers;
  std::vector<type::Type> type_stack;
  std::vector<Token::Kind> operator_stack;
  std::vector<Module> modules;
  EmitContext emit;
};

}  // namespace ic

#endif  // ICARUS_IR_IR_H
