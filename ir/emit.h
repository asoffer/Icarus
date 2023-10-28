#ifndef ICARUS_IR_EMIT_H
#define ICARUS_IR_EMIT_H

#include <span>
#include <vector>

#include "absl/container/btree_map.h"
#include "absl/container/flat_hash_map.h"
#include "common/module_id.h"
#include "common/identifier.h"
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
                       Module& module)
      : tree(tree), current_module{module}, modules(modules) {
    types_.reserve(tree.size());
  }

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

  ParseTree::Node const& Node(ParseTree::Node::Index index) const {
    return tree[index];
  }

  ParseTree const& tree;

  absl::flat_hash_map<ParseTree::Node::Index, type::QualifiedType>
      statement_qualified_type;

  absl::flat_hash_map<ParseTree::Node::Index, size_t> rotation_count;
  absl::flat_hash_map<ParseTree::Node::Index,
                      std::pair<ParseTree::Node::Index, ParseTree::Node::Index>>
      declarator;
  absl::flat_hash_map<Identifier,
                      std::tuple<ParseTree::Node::Index, ParseTree::Node::Index,
                                 type::QualifiedType>>
      identifiers;
  absl::flat_hash_set<ParseTree::Node::Index> declarations_to_export;
  Module& current_module;

  void set_current_function(IrFunction& f) {
    NTH_REQUIRE((v.debug), not queue.empty());
    queue.front().function = &f;
  }

  IrFunction& current_function() {
    NTH_REQUIRE((v.debug), not queue.empty());
    NTH_REQUIRE((v.debug), queue.front().function != nullptr);
    return *queue.front().function;
  }

  // Maps node indices to the constant value associated with the computation for
  // the largest subtree containing it whose constant value has been computed
  // thus far.
  nth::interval_map<ParseTree::Node::Index, ComputedConstants> constants;
  DependentModules const& modules;
  struct WorkItem {
    IrFunction* function = nullptr;
    nth::interval<ParseTree::Node::Index> range;
    std::vector<DeclarationInfo> declaration_stack;
  };
  std::queue<WorkItem> queue;

  void SetQualifiedType(ParseTree::Node::Index index, type::QualifiedType qt) {
    types_[index.value()] = qt;
  }
  type::QualifiedType QualifiedTypeOf(ParseTree::Node::Index index) {
    return types_[index.value()];
  }

 private:
  std::vector<type::QualifiedType> types_;
};

void EmitIr(EmitContext& context);

void SetExported(EmitContext const& context);

}  // namespace ic

#endif  // ICARUS_IR_EMIT_H
