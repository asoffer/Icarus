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
#include "ir/builder.h"

namespace compiler {
struct Compiler;  // TODO move into it's own header.
}  // namespace compiler

namespace compiler::internal {

struct TableImpl {
  static base::expected<TableImpl> Verify(
      Compiler *compiler, ast::OverloadSet const &os,
      core::FnArgs<VerifyResult> const &args);

  struct ExprData {
    type::Type const *type;
    core::FnParams<type::Type const *> params;
  };

  absl::flat_hash_map<ast::Expression const *, ExprData> table_;
};
}  // namespace compiler::internal

namespace compiler {

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
      ir::Builder &builder,
      core::FnArgs<std::pair<type::Typed<ast::Expression const *>,
                             ir::Results>> const &args) const;

 private:
  static type::Type const *ComputeResultType(internal::TableImpl const &impl);

  internal::TableImpl impl_;
  type::Type const *result_type_;
};

struct JumpDispatchTable {
  static base::expected<JumpDispatchTable> Verify(
      Compiler *compiler, ast::OverloadSet const &os,
      core::FnArgs<VerifyResult> const &args) {
    ASSIGN_OR(return _.error(),  //
                     auto impl,
                     internal::TableImpl::Verify(compiler, os, args));
    JumpDispatchTable table;
    table.impl_ = std::move(impl);
    return table;
  }

 private:
  internal::TableImpl impl_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_TABLE_H
