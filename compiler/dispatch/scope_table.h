#ifndef ICARUS_COMPILER_DISPATCH_SCOPE_TABLE_H
#define ICARUS_COMPILER_DISPATCH_SCOPE_TABLE_H

#include <optional>

#include "absl/container/flat_hash_map.h"
#include "ast/overload_set.h"
#include "base/expected.h"
#include "compiler/dispatch/jump_table.h"
#include "compiler/dispatch/match.h"
#include "compiler/dispatch/overload.h"
#include "compiler/verify_result.h"
#include "core/fn_args.h"
#include "core/fn_params.h"
#include "ir/jump.h"
#include "ir/scope_def.h"

namespace compiler {
struct Compiler;  // TODO move into it's own header.

namespace internal {

struct OneTable {
  absl::flat_hash_map<ir::Jump const *,
                      core::FnParams<type::Typed<ast::Declaration const *>>>
      inits;
  absl::flat_hash_map<ast::BlockNode const *, JumpDispatchTable> blocks;
};

}  // namespace internal

struct ScopeDispatchTable {
  static base::expected<ScopeDispatchTable> Verify(
      Compiler *compiler, ast::ScopeNode const *node,
      absl::flat_hash_map<ir::Jump const *, ir::ScopeDef const *> inits,
      core::FnArgs<VerifyResult> const &args);

  ir::Results EmitCall(
      Compiler *compiler,
      core::FnArgs<type::Typed<ir::Results>> const &args) const;

 private:
  absl::flat_hash_map<ir::Jump const*, ir::ScopeDef const *> init_map_;
  absl::flat_hash_map<ir::ScopeDef const *, internal::OneTable> tables_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_SCOPE_TABLE_H
