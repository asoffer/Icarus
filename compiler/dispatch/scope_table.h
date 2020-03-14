#ifndef ICARUS_COMPILER_DISPATCH_SCOPE_TABLE_H
#define ICARUS_COMPILER_DISPATCH_SCOPE_TABLE_H

#include <optional>

#include "absl/container/flat_hash_map.h"
#include "ast/overload_set.h"
#include "base/expected.h"
#include "compiler/dispatch/jump_table.h"
#include "compiler/dispatch/match.h"
#include "compiler/dispatch/overload.h"
#include "core/fn_args.h"
#include "core/params.h"
#include "ir/jump.h"
#include "ir/scope_def.h"
#include "type/qual_type.h"

namespace compiler {
struct Compiler;  // TODO move into it's own header.

namespace internal {

struct OneTable {
  absl::Span<type::Type const *const> result_types() const {
    return result_types_;
  }

  void VerifyBlocks(Compiler *compiler, ast::ScopeNode const *node);
  void VerifyJumps();

  void EmitCall(Compiler *compiler, ir::ScopeDef const *scope_def,
                std::optional<ir::Reg> state_reg,
                ir::LocalBlockInterpretation const &block_interp) const;

  absl::flat_hash_map<ir::Jump *, core::Params<type::Type const *>> inits;
  absl::flat_hash_map<ast::BlockNode const *, JumpDispatchTable> blocks;
  ir::ScopeDef const *scope_def_;
  std::vector<type::Type const *> result_types_;
};

}  // namespace internal

struct ScopeDispatchTable {
  static base::expected<ScopeDispatchTable> Verify(
      Compiler *compiler, ast::ScopeNode const *node,
      absl::flat_hash_map<ir::Jump *, ir::ScopeDef const *> inits,
      core::FnArgs<type::Typed<ir::Results>> const &args);

  type::QualType qual_type() const { return qual_type_; }

  ir::Results EmitCall(
      Compiler *compiler,
      core::FnArgs<type::Typed<ir::Results>> const &args) const;

 private:
  // Given a value of type `A | B` passed into the scope, there may be no
  // specific scope in the scope overload set which accepts an initial value of
  // this type, but there could be scopes separately that accept values of type
  // `A` and `B`. Just as we do with function dispatch over variants, we will
  // dispatch to separate scopes. Note that this only happens on initial scope
  // entry. We cannot jump between blocks associated with a scope instantiated
  // for `A` and blocks associated with a scope instantiated for `B`.
  void EmitSplittingDispatch(
      Compiler *compiler,
      absl::flat_hash_map<ir::ScopeDef const *, ir::Reg> const &state_regs,
      absl::flat_hash_map<ir::ScopeDef const *,
                          ir::LocalBlockInterpretation> const &block_interps,
      core::FnArgs<type::Typed<ir::Results>> const &args) const;

  ast::ScopeNode const *scope_node_;
  absl::flat_hash_map<ir::Jump *, ir::ScopeDef const *> init_map_;
  // TODO this is really more of a hash_set. We only use the lookup
  // functionality once when we create it.
  absl::flat_hash_map<ir::ScopeDef const *, internal::OneTable> tables_;
  type::QualType qual_type_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_SCOPE_TABLE_H
