#ifndef ICARUS_IR_VALUE_SCOPE_H
#define ICARUS_IR_VALUE_SCOPE_H

#include <string>
#include <vector>

#include "absl/types/span.h"
#include "base/any_invocable.h"
#include "base/extend.h"
#include "base/extend/absl_format.h"
#include "base/extend/absl_hash.h"
#include "base/extend/serialize.h"
#include "compiler/work_resources.h"
#include "core/params.h"
#include "ir/blocks/group.h"
#include "ir/value/block.h"
#include "ir/value/reg.h"
#include "ir/value/result_buffer.h"
#include "type/scope.h"

namespace ir {

using CompiledScope = BlockGroup<type::Scope>;

struct Scope : base::Extend<Scope, 1>::With<base::AbslFormatExtension,
                                            base::AbslHashExtension> {
  static constexpr std::string_view kAbslFormatString = "Scope(%p)";

  struct Data {
    CompiledScope *scope;
    type::Scope const *type;
    std::vector<std::pair<BasicBlock *, BasicBlock *>> connections;
    absl::flat_hash_map<Block, std::vector<Reg>> parameters;
    base::untyped_buffer::const_iterator byte_code;
  };

  constexpr Scope() : data_(nullptr) {}
  explicit constexpr Scope(Scope::Data *data) : data_(ASSERT_NOT_NULL(data)) {}

  void add_connection(BasicBlock *entry_to_block, BasicBlock *exit_from_block) {
    data_->connections.emplace_back(entry_to_block, exit_from_block);
  }
  std::pair<BasicBlock *, BasicBlock *> const &connection(Block b) const {
    return data_->connections[b.value()];
  }

  CompiledScope *operator->() { return get().scope; }
  CompiledScope &operator*() { return *get().scope; }

  void add_parameters(Block b, Reg r) {
    return data_->parameters[b].push_back(r);
  }
  absl::Span<Reg const> parameters(Block b) { return data_->parameters[b]; }

  type::Scope const *type() const {
    return ASSERT_NOT_NULL(get().type);
  }

 private:
  friend CompiledScope;
  friend base::EnableExtensions;

  Data const &get() const { return *ASSERT_NOT_NULL(data_); }

  Data *data_;
};

struct ScopeContext
    : base::Extend<ScopeContext, 1>::With<base::BaseSerializeExtension> {
  // TODO: Rather than storing the parameters, we should store the actual
  // block-type, or, if it's generic, the generic that could be used to
  // instantiate a block-type.
  using block_type = std::pair<std::string, core::Params<type::QualType>>;

  ScopeContext() : block_names_(nullptr) {}
  explicit ScopeContext(std::vector<block_type> const *block_names)
      : block_names_(ASSERT_NOT_NULL(block_names)) {}

  absl::Span<block_type const> blocks() const { return *block_names_; }

  size_t size() const { return block_names_->size(); }

  Block find(std::string_view name) const {
    for (size_t i = 0; i < block_names_->size(); ++i) {
      auto const &[block_name, qts] = (*block_names_)[i];
      if (block_name == name) { return Block(i); }
    }
    return Block::Invalid();
  }

  auto const &operator[](Block b) const {
    ASSERT(b != Block::Invalid());
    return (*block_names_)[b.value()];
  }

  friend bool operator==(ScopeContext const &lhs, ScopeContext const &rhs) {
    return *lhs.block_names_ == *rhs.block_names_;
  }

  friend bool operator!=(ScopeContext const &lhs, ScopeContext const &rhs) {
    return not(lhs == rhs);
  }

 private:
  friend base::EnableExtensions;
  std::vector<block_type> const *block_names_;
};

struct UnboundScope
    : base::Extend<UnboundScope, 1>::With<base::AbslFormatExtension,
                                          base::AbslHashExtension> {
  static constexpr std::string_view kAbslFormatString = "UnboundScope(%p)";

  explicit UnboundScope(
      base::any_invocable<
          Scope(compiler::WorkResources const &, ScopeContext const &,
                core::Arguments<type::Typed<CompleteResultRef>> const &)> *f =
          nullptr)
      : f_(f) {}

  Scope bind(compiler::WorkResources const &wr, ScopeContext const &ctx,
             core::Arguments<type::Typed<CompleteResultRef>> const &arguments) const {
    return (*f_)(wr, ctx, arguments);
  }

 private:
  friend base::EnableExtensions;

  base::any_invocable<Scope(
      compiler::WorkResources const &, ScopeContext const &,
      core::Arguments<type::Typed<CompleteResultRef>> const &)> *f_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_SCOPE_H
