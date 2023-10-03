#ifndef ICARUS_IR_EMIT_H
#define ICARUS_IR_EMIT_H

#include <span>
#include <vector>

#include "absl/container/btree_map.h"
#include "absl/container/flat_hash_map.h"
#include "ir/dependent_modules.h"
#include "ir/module.h"
#include "ir/module_id.h"
#include "jasmin/value_stack.h"
#include "lexer/token_buffer.h"
#include "nth/base/attributes.h"
#include "parser/parse_tree.h"
#include "type/type.h"

namespace ic {

struct EmitContext {
  explicit EmitContext(
      ParseTree const& tree NTH_ATTRIBUTE(lifetimebound),
      TokenBuffer const& token_buffer NTH_ATTRIBUTE(lifetimebound),
      DependentModules const& modules NTH_ATTRIBUTE(lifetimebound),
      IrFunction& f)
      : tree(tree),
        function_stack{&f},
        token_buffer(token_buffer),
        modules(modules) {}
  explicit EmitContext(
      ParseTree const& tree NTH_ATTRIBUTE(lifetimebound),
      TokenBuffer const& token_buffer NTH_ATTRIBUTE(lifetimebound),
      DependentModules const& modules NTH_ATTRIBUTE(lifetimebound),
      Module& module)
      : tree(tree),
        function_stack{&module.initializer()},
        token_buffer(token_buffer),
        modules(modules) {}

  Module const& module(ModuleId id) const { return modules[id]; }

  void Push(jasmin::Value v, type::Type);

  ParseTree::Node const& Node(ParseTree::Node::Index index) {
    return tree[index];
  }

  ParseTree const& tree;

  absl::flat_hash_map<ParseTree::Node, type::QualifiedType>
      statement_qualified_type;
  std::vector<Token::Kind> operator_stack;
  std::vector<IrFunction*> function_stack;

  struct Compare {
    bool operator()(nth::interval<ParseTree::Node::Index> const& lhs,
                    nth::interval<ParseTree::Node::Index> const& rhs) const {
      auto const& [lhs_l, lhs_u] = lhs;
      auto const& [rhs_l, rhs_u] = rhs;
      if (lhs_l < rhs_l) { return true; }
      if (lhs_l > rhs_l) { return false; }
      return lhs_u < rhs_u;
    }
  };

  // Indices covering subtree roots which were required to be constant evaluated
  // in order to type-check their parent, mapped to their corresponding constant
  // value.
  //
  // TODO: This should really be it's own interval map type.
  absl::btree_map<nth::interval<ParseTree::Node::Index>, jasmin::ValueStack, Compare>
      constants;
  TokenBuffer const & token_buffer;
  DependentModules const & modules;
};

void EmitIr(nth::interval<ParseTree::Node::Index> node_range, EmitContext& context);

void Evaluate(nth::interval<ParseTree::Node::Index> subtree,
              ParseTree const& tree,
              TokenBuffer const& token_buffer NTH_ATTRIBUTE(lifetimebound),
              DependentModules const& modules NTH_ATTRIBUTE(lifetimebound),
              jasmin::ValueStack& value_stack);

}  // namespace ic

#endif  // ICARUS_IR_EMIT_H
