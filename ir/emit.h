#ifndef ICARUS_IR_EMIT_H
#define ICARUS_IR_EMIT_H

#include <span>
#include <vector>

#include "absl/container/btree_map.h"
#include "absl/container/flat_hash_map.h"
#include "common/identifier.h"
#include "common/module_id.h"
#include "ir/dependent_modules.h"
#include "ir/local_storage.h"
#include "ir/module.h"
#include "ir/scope.h"
#include "jasmin/value_stack.h"
#include "nth/base/attributes.h"
#include "nth/container/interval_map.h"
#include "parse/node_index.h"
#include "parse/tree.h"
#include "type/type.h"

namespace ic {

struct DeclarationInfo {
  Token::Kind kind     = Token::Kind::Invalid;
  ParseNodeIndex index = ParseNodeIndex::Invalid();
};

struct EmitContext {
  explicit EmitContext(ParseTree const& tree NTH_ATTRIBUTE(lifetimebound),
                       DependentModules const& modules
                           NTH_ATTRIBUTE(lifetimebound),
                       ScopeTree& scopes, Module& module)
      : tree(tree), scopes(scopes), current_module{module}, modules(modules) {
    types_.reserve(tree.size());
  }

  Module const& module(ModuleId id) const { return modules[id]; }

  struct ComputedConstants {
    explicit ComputedConstants(ParseNodeIndex index, jasmin::ValueStack value,
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
    ParseNodeIndex index_;
    jasmin::ValueStack value_;
    std::vector<type::Type> types_;
  };

  void Push(std::span<jasmin::Value const>, type::Type);
  void Push(std::span<jasmin::Value const>, std::span<type::Type const>);
  void Push(ComputedConstants const& c);

  void Evaluate(nth::interval<ParseNodeIndex> subtree,
                jasmin::ValueStack& value_stack, std::vector<type::Type> types);

  ParseNode const& Node(ParseNodeIndex index) const { return tree[index]; }

  ParseTree const& tree;

  absl::flat_hash_map<ParseNodeIndex, type::QualifiedType>
      statement_qualified_type;

  absl::flat_hash_map<ParseNodeIndex, size_t> rotation_count;
  absl::flat_hash_map<ParseNodeIndex, std::pair<ParseNodeIndex, ParseNodeIndex>>
      declarator;

  ScopeTree& scopes;
  absl::flat_hash_set<ParseNodeIndex> declarations_to_export;
  absl::flat_hash_map<Scope::Index, LocalStorage> storage;
  Module& current_module;

  void set_current_function(IrFunction& f) {
    NTH_REQUIRE((v.debug), not queue.empty());
    queue.front().function = &f;
  }

  IrFunction& current_function() {
    NTH_REQUIRE((v.harden), not queue.empty());
    NTH_REQUIRE((v.debug), queue.front().function != nullptr);
    return *queue.front().function;
  }

  void pop_scope() {
    NTH_REQUIRE((v.harden), not queue.empty());
    queue.front().scopes.pop_back();
  }

  void push_scope(Scope::Index index) { queue.front().scopes.push_back(index); }

  Scope::Index current_scope_index() {
    NTH_REQUIRE((v.harden), not queue.front().scopes.empty());
    return queue.front().scopes.back();
  }

  LocalStorage &current_storage() {
    NTH_REQUIRE((v.harden), not queue.front().function_stack.empty());
    return storage[queue.front().function_stack.back()];
  }

  // Maps node indices to the constant value associated with the computation for
  // the largest subtree containing it whose constant value has been computed
  // thus far.
  nth::interval_map<ParseNodeIndex, ComputedConstants> constants;
  DependentModules const& modules;
  enum class ValueCategory : uint8_t {
    Value,
    Reference,
  };
  struct WorkItem {
    IrFunction* function = nullptr;
    nth::interval<ParseNodeIndex> range;
    std::vector<DeclarationInfo> declaration_stack;
    std::vector<jasmin::OpCodeRange> branches;
    std::vector<Scope::Index> scopes = {Scope::Index::Root()};
    std::vector<Scope::Index> function_stack;
    std::vector<ValueCategory> value_category_stack;
  };
  std::queue<WorkItem> queue;

  void SetQualifiedType(ParseNodeIndex index, type::QualifiedType qt) {
    types_[index.value()] = qt;
  }
  type::QualifiedType QualifiedTypeOf(ParseNodeIndex index) {
    return types_[index.value()];
  }

 private:
  std::vector<type::QualifiedType> types_;
};

void EmitIr(EmitContext& context);

void SetExported(EmitContext const& context);

}  // namespace ic

#endif  // ICARUS_IR_EMIT_H
