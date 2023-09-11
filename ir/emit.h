#ifndef ICARUS_IR_EMIT_H
#define ICARUS_IR_EMIT_H

#include <span>

#include "absl/container/flat_hash_map.h"
#include "ir/module.h"
#include "parser/parse_tree.h"
#include "type/type.h"

namespace ic {

struct EmitContext {
  explicit EmitContext(IrFunction& f) : function_stack{&f} {}
  explicit EmitContext(Module& module)
      : function_stack{&module.initializer()} {}

  absl::flat_hash_map<ParseTree::Node, type::Type> statement_type;
  std::vector<Token::Kind> operator_stack;
  std::vector<IrFunction*> function_stack;
};

void EmitIr(std::span<ParseTree::Node const> nodes, EmitContext& context);

void Evaluate(std::span<ParseTree::Node const> subtree,
              jasmin::ValueStack& value_stack);

}  // namespace ic

#endif  // ICARUS_IR_EMIT_H
