#ifndef ICARUS_COMPILER_DISPATCH_SCOPE_TABLE_H
#define ICARUS_COMPILER_DISPATCH_SCOPE_TABLE_H

#include <optional>

#include "absl/container/flat_hash_map.h"
#include "ast/overload_set.h"
#include "base/expected.h"
#include "compiler/dispatch/match.h"
#include "compiler/dispatch/overload.h"
#include "compiler/verify_result.h"
#include "core/fn_args.h"
#include "core/fn_params.h"
#include "ir/jump.h"
#include "ir/scope_def.h"

namespace compiler {
struct Compiler;  // TODO move into it's own header.

struct ScopeDispatchTable {
  static base::expected<ScopeDispatchTable> Verify(
      Compiler *compiler, ast::ScopeNode const *node,
      absl::flat_hash_map<ir::Jump const *, ir::ScopeDef const *> inits,
      core::FnArgs<VerifyResult> const &args);

 private:
  struct JumpDispatchTable {
    static base::expected<JumpDispatchTable> Verify(
        Compiler *compiler, ast::ScopeNode const *node,
        absl::Span<ir::Jump const *const> jumps,
        core::FnArgs<VerifyResult> const &args);

   private:
    absl::flat_hash_map<ir::Jump const *, internal::ExprData> table_;
  };

  absl::flat_hash_map<
      ir::ScopeDef const *,
      absl::flat_hash_map<ir::Jump const *, core::FnParams<type::Typed<
                                                ast::Declaration const *>>>>
      init_table_;

  absl::flat_hash_map<
      ir::ScopeDef const *,
      absl::flat_hash_map<ast::BlockNode const *, JumpDispatchTable>>
      block_tables_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_SCOPE_TABLE_H
