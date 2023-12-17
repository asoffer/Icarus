#ifndef ICARUS_IR_EMIT_H
#define ICARUS_IR_EMIT_H

#include <queue>
#include <span>
#include <vector>

#include "absl/container/btree_map.h"
#include "absl/container/flat_hash_map.h"
#include "common/identifier.h"
#include "common/module_id.h"
#include "ir/dependent_modules.h"
#include "ir/lexical_scope.h"
#include "ir/local_storage.h"
#include "ir/module.h"
#include "nth/base/attributes.h"
#include "nth/container/interval.h"
#include "nth/container/interval_map.h"
#include "nth/container/stack.h"
#include "parse/declaration.h"
#include "parse/node_index.h"
#include "parse/tree.h"
#include "type/type.h"

namespace ic {

struct Iteration {
  enum class Kind : uint32_t { Continue, PauseRetry, PauseMoveOn, Skip };
  using enum Kind;

  Iteration(Kind k) : Iteration(k, ParseNodeIndex::Invalid()) {
    NTH_REQUIRE((v.debug), k != Kind::Skip);
  }

  constexpr Kind kind() const { return static_cast<Kind>(value_); }
  constexpr ParseNodeIndex index() const { return index_; }

  static constexpr Iteration SkipTo(ParseNodeIndex index) {
    return Iteration(static_cast<Kind>(Kind::Skip), index);
  }

 private:
  constexpr Iteration(Kind k, ParseNodeIndex index)
      : value_(static_cast<uint32_t>(k)), index_(index) {}
  uint32_t value_;
  ParseNodeIndex index_;
};

struct EmitContext {
  explicit EmitContext(ParseTree const& tree NTH_ATTRIBUTE(lifetimebound),
                       DependentModules const& modules
                           NTH_ATTRIBUTE(lifetimebound),
                       LexicalScopeTree& scopes, Module& module)
      : tree(tree), lexical_scopes(scopes), current_module{module}, modules(modules) {
    types_.reserve(tree.size());
  }

  Module const& module(ModuleId id) const { return modules[id]; }

  struct ComputedConstants {
    explicit ComputedConstants(ParseNodeIndex index,
                               nth::stack<jasmin::Value> value,
                               std::vector<type::Type> types)
        : index_(index), value_(std::move(value)), types_(std::move(types)) {}

    friend bool operator==(ComputedConstants const& lhs,
                           ComputedConstants const& rhs) {
      return lhs.index_ == rhs.index_;
    }

    std::span<type::Type const> types() const { return types_; }
    std::span<jasmin::Value const> value_span() const {
      return value_.top_span(value_.size());
    }

   private:
    ParseNodeIndex index_;
    nth::stack<jasmin::Value> value_;
    std::vector<type::Type> types_;
  };

  void Push(std::span<jasmin::Value const>, type::Type);
  void Push(std::span<jasmin::Value const>, std::span<type::Type const>);
  void Push(ComputedConstants const& c);

  void Evaluate(nth::interval<ParseNodeIndex> subtree,
                nth::stack<jasmin::Value>& value_stack, std::vector<type::Type> types);

  ParseNode const& Node(ParseNodeIndex index) const { return tree[index]; }

  ParseTree const& tree;

  absl::flat_hash_map<ParseNodeIndex, std::pair<type::ByteWidth, size_t>>
      statement_expression_info;

  absl::flat_hash_map<ParseNodeIndex, jasmin::InstructionSpecification>
      instruction_spec;
  absl::flat_hash_map<ParseNodeIndex, std::pair<ParseNodeIndex, ParseNodeIndex>>
      declarator;

  LexicalScopeTree& lexical_scopes;
  absl::flat_hash_set<ParseNodeIndex> declarations_to_export;
  absl::flat_hash_map<LexicalScope::Index, LocalStorage> storage;
  Module& current_module;

  void push_function(IrFunction& f, LexicalScope::Index scope_index) {
    queue.front().push_function(f, scope_index);
  }

  void pop_function() {
    NTH_REQUIRE((v.debug), not queue.empty());
    NTH_REQUIRE((v.debug), not queue.front().function_stack_.empty());
    NTH_REQUIRE((v.debug), not queue.front().function_stack.empty());
    NTH_REQUIRE((v.debug), not queue.front().lexical_scopes.empty());
    queue.front().function_stack_.pop_back();
    queue.front().function_stack.pop_back();
    queue.front().lexical_scopes.pop_back();
  }

  IrFunction& current_function() {
    NTH_REQUIRE((v.harden), not queue.empty());
    NTH_REQUIRE((v.debug), not queue.front().function_stack_.empty());
    NTH_REQUIRE((v.debug), queue.front().function_stack_.back() != nullptr);
    return *queue.front().function_stack_.back();
  }

  void push_scope(Scope& f, LexicalScope::Index scope_index) {
    queue.front().push_scope(f, scope_index);
  }

  void pop_scope() {
    NTH_REQUIRE((v.debug), not queue.empty());
    NTH_REQUIRE((v.debug), not queue.front().scope_stack_.empty());
    NTH_REQUIRE((v.debug), not queue.front().function_stack.empty());
    NTH_REQUIRE((v.debug), not queue.front().function_stack_.empty());
    NTH_REQUIRE((v.debug), not queue.front().lexical_scopes.empty());
    queue.front().scope_stack_.pop_back();
    queue.front().function_stack.pop_back();
    queue.front().function_stack_.pop_back();
    queue.front().lexical_scopes.pop_back();
  }

  Scope& current_scope() {
    NTH_REQUIRE((v.harden), not queue.empty());
    NTH_REQUIRE((v.debug), not queue.front().scope_stack_.empty());
    NTH_REQUIRE((v.debug), queue.front().scope_stack_.back() != nullptr);
    return *queue.front().scope_stack_.back();
  }

  void pop_lexical_scope() {
    NTH_REQUIRE((v.harden), not queue.empty());
    queue.front().lexical_scopes.pop_back();
  }

  void push_lexical_scope(LexicalScope::Index index) {
    queue.front().lexical_scopes.push_back(index);
  }

  LexicalScope::Index current_lexical_scope_index() {
    NTH_REQUIRE((v.harden), not queue.front().lexical_scopes.empty());
    return queue.front().lexical_scopes.back();
  }

  LocalStorage& current_storage() {
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
    void push_function(IrFunction& f, LexicalScope::Index scope_index) {
      function_stack_.push_back(&f);
      function_stack.push_back(scope_index);
      lexical_scopes.push_back(scope_index);
    }

    void push_scope(Scope& s, LexicalScope::Index scope_index) {
      scope_stack_.push_back(&s);
      function_stack_.push_back(&s.implementation());
      function_stack.push_back(scope_index);
      lexical_scopes.push_back(scope_index);
    }

    nth::interval<ParseNodeIndex> range;
    std::vector<DeclarationInfo> declaration_stack;
    std::vector<nth::interval<jasmin::InstructionIndex>> branches;
    std::vector<LexicalScope::Index> lexical_scopes = {LexicalScope::Index::Root()};
    std::vector<LexicalScope::Index> function_stack = {
        LexicalScope::Index::Root()};
    std::vector<ValueCategory> value_category_stack;

    // TODO: Make private (requires no longer using designated initializers).
    // TODO: Combine these and check access to them.
    std::vector<IrFunction*> function_stack_;
    std::vector<Scope*> scope_stack_;
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
