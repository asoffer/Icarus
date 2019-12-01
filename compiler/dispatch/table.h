#ifndef ICARUS_COMPILER_DISPATCH_TABLE_H
#define ICARUS_COMPILER_DISPATCH_TABLE_H

#include <optional>

#include "absl/container/flat_hash_map.h"
#include "ast/overload_set.h"
#include "base/expected.h"
#include "compiler/dispatch/match.h"
#include "compiler/verify_result.h"
#include "core/fn_args.h"
#include "core/fn_params.h"
#include "ir/jump.h"
#include "ir/scope_def.h"

namespace compiler {
struct Compiler;  // TODO move into it's own header.
}  // namespace compiler

namespace compiler::internal {
struct ExprData {
  type::Type const *type;
  core::FnParams<type::Typed<ast::Declaration const *>> params;
};

struct TableImpl {
  static base::expected<TableImpl> Verify(
      Compiler *compiler, ast::OverloadSet const &os,
      core::FnArgs<VerifyResult> const &args);

  absl::flat_hash_map<ast::Expression const *, ExprData> table_;
};
}  // namespace compiler::internal

namespace compiler {

// TODO it looks like we actually won't be sharing much between function-call
// and jump in terms of the interface. Should probably inline TableImpl and
// separate out these headers.
struct FnCallDispatchTable {
  static base::expected<FnCallDispatchTable> Verify(
      Compiler *compiler, ast::OverloadSet const &os,
      core::FnArgs<VerifyResult> const &args) {
    ASSIGN_OR(return _.error(),  //
                     auto impl,
                     internal::TableImpl::Verify(compiler, os, args));
    FnCallDispatchTable table;
    table.impl_        = std::move(impl);
    table.result_type_ = ComputeResultType(table.impl_);
    return table;
  }
  type::Type const *result_type() const { return result_type_; }

  ir::Results EmitCall(
      Compiler *compiler,
      core::FnArgs<type::Typed<ir::Results>> const &args) const;

 private:
  static type::Type const *ComputeResultType(internal::TableImpl const &impl);

  internal::TableImpl impl_;
  type::Type const *result_type_;
};

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

#endif  // ICARUS_COMPILER_DISPATCH_TABLE_H
