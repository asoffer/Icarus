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
#include "core/fn_params.h"
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

  absl::flat_hash_map<ir::Jump *,
                      core::FnParams<type::Typed<ast::Declaration const *>>>
      inits;
  absl::flat_hash_map<ast::BlockNode const *, JumpDispatchTable> blocks;
  ir::ScopeDef const *scope_def_;
  std::vector<type::Type const *> result_types_;
};

}  // namespace internal

struct ScopeDispatchTable {
  static base::expected<ScopeDispatchTable> Verify(
      Compiler *compiler, ast::ScopeNode const *node,
      absl::flat_hash_map<ir::Jump *, ir::ScopeDef const *> inits,
      core::FnArgs<type::QualType> const &args);

  type::QualType qual_type() const { return qual_type_; }

  ir::Results EmitCall(
      Compiler *compiler,
      core::FnArgs<type::Typed<ir::Results>> const &args) const;

 private:
  ast::ScopeNode const *scope_node_;
  absl::flat_hash_map<ir::Jump *, ir::ScopeDef const *> init_map_;
  // TODO this is really more of a hash_set. We only use the lookup
  // functionality once when we create it.
  absl::flat_hash_map<ir::ScopeDef const *, internal::OneTable> tables_;
  type::QualType qual_type_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_SCOPE_TABLE_H
