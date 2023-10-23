#ifndef ICARUS_IR_EMIT_H
#define ICARUS_IR_EMIT_H

#include <span>
#include <vector>

#include "absl/container/btree_map.h"
#include "absl/container/flat_hash_map.h"
#include "common/module_id.h"
#include "ir/dependent_modules.h"
#include "ir/module.h"
#include "jasmin/value_stack.h"
#include "nth/base/attributes.h"
#include "nth/container/interval_map.h"
#include "parser/parse_tree.h"
#include "type/type.h"

namespace ic {

struct DeclarationInfo {
  Token::Kind kind             = Token::Kind::Invalid;
  ParseTree::Node::Index index = ParseTree::Node::Index::Invalid();
};

struct EmitContext {
  explicit EmitContext(ParseTree const& tree NTH_ATTRIBUTE(lifetimebound),
                       DependentModules const& modules
                           NTH_ATTRIBUTE(lifetimebound),
                       IrFunction& f)
      : tree(tree), function_stack{&f}, modules(modules) {}
  explicit EmitContext(ParseTree const& tree NTH_ATTRIBUTE(lifetimebound),
                       DependentModules const& modules
                           NTH_ATTRIBUTE(lifetimebound),
                       Module& module)
      : tree(tree), function_stack{&module.initializer()}, modules(modules) {}

  Module const& module(ModuleId id) const { return modules[id]; }

  struct ComputedConstants {
    explicit ComputedConstants(ParseTree::Node::Index index,
                               jasmin::ValueStack value,
                               std::vector<type::Type> types)
        : index_(index), value_(std::move(value)), types_(std::move(types)) {}

    friend bool operator==(ComputedConstants const& lhs,
                           ComputedConstants const& rhs) {
      return lhs.index_ == rhs.index_;
    }

    std::span<type::Type const> types() const { return types_; }
    std::span<jasmin::Value const> value_span() const {
      return std::span<jasmin::Value const>(value_.begin(), value_.end());
    }

   private:
    ParseTree::Node::Index index_;
    jasmin::ValueStack value_;
    std::vector<type::Type> types_;
  };

  void Push(std::span<jasmin::Value const>, type::Type);
  void Push(std::span<jasmin::Value const>, std::span<type::Type const>);
  void Push(ComputedConstants const& c);

  void Evaluate(nth::interval<ParseTree::Node::Index> subtree,
                jasmin::ValueStack& value_stack, std::vector<type::Type> types);

  ParseTree::Node const& Node(ParseTree::Node::Index index) {
    return tree[index];
  }

  ParseTree const& tree;

  absl::flat_hash_map<ParseTree::Node::Index, type::QualifiedType>
      statement_qualified_type;

  std::vector<std::unique_ptr<IrFunction>> temporary_functions;
  std::vector<Token::Kind> operator_stack;
  std::vector<IrFunction*> function_stack;
  absl::flat_hash_map<ParseTree::Node::Index, size_t> rotation_count;
  absl::flat_hash_map<ParseTree::Node::Index,
                      std::pair<ParseTree::Node::Index, ParseTree::Node::Index>>
      declarator;
  absl::flat_hash_map<uint32_t,
                      std::tuple<ParseTree::Node::Index, ParseTree::Node::Index,
                                 type::QualifiedType>>
      identifiers;

  // Maps node indices to the constant value associated with the computation for
  // the largest subtree containing it whose constant value has been computed
  // thus far.
  nth::interval_map<ParseTree::Node::Index, ComputedConstants> constants;
  DependentModules const& modules;
  struct WorkItem {
    nth::interval<ParseTree::Node::Index> range;
    std::vector<DeclarationInfo> declaration_stack;
  };
  std::queue<WorkItem> queue;
};

void EmitIr(EmitContext& context);

}  // namespace ic

#endif  // ICARUS_IR_EMIT_H
