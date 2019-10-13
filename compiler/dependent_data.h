#ifndef ICARUS_COMPILER_DEPENDENT_DATA_H
#define ICARUS_COMPILER_DEPENDENT_DATA_H

#include "absl/container/flat_hash_map.h"
#include "absl/container/node_hash_map.h"
#include "ast/ast_fwd.h"
#include "ast/dispatch_table.h"
#include "ast/expr_ptr.h"
#include "compiler/constant_binding.h"
#include "compiler/verify_result.h"
#include "ir/scope_def.h"
#include "module/pending.h"

namespace ir {
struct CompiledFn;
}  // namespace ir

namespace compiler {
struct DependentData {
  absl::flat_hash_map<ast::Declaration const *, ir::Reg> addr_;

  // TODO probably make these funcs constant.
  absl::node_hash_map<ast::Expression const *, ir::CompiledFn *> ir_funcs_;

  absl::flat_hash_map<ast::ExprPtr, ast::DispatchTable> dispatch_tables_;

  // Similar to dispatch tables, but specifically for `jump_handler`s. The
  // tables are keyed on both the scope/block node as well as the actual jump
  // expression.
  absl::flat_hash_map<std::pair<ast::ExprPtr, ast::ExprPtr>, ast::DispatchTable>
      jump_tables_;
  absl::node_hash_map<ast::ScopeLiteral const *, ir::ScopeDef> scope_defs_;

  compiler::ConstantBinding constants_;

  absl::flat_hash_map<ast::Import const *, module::PendingModule>
      imported_module_;

  VerifyResult const *result(ast::ExprPtr expr) const {
    auto iter = type_verification_results_.find(expr);
    return iter == type_verification_results_.end() ? nullptr : &iter->second;
  }

  VerifyResult set_result(ast::ExprPtr expr, VerifyResult r) {
    type_verification_results_.emplace(expr, r);
    return r;
  }

 private:
  absl::flat_hash_map<ast::ExprPtr, VerifyResult> type_verification_results_;
};
}  // namespace compiler

#endif  // ICARUS_COMPILER_DEPENDENT_DATA_H
