#ifndef ICARUS_MISC_DEPENDENT_DATA_H
#define ICARUS_MISC_DEPENDENT_DATA_H

#include "absl/container/flat_hash_map.h"
#include "absl/container/node_hash_map.h"
#include "ast/expr_ptr.h"
#include "misc/constant_binding.h"

// TODO this ifdef needs to disappear it's not long-term sustainable
#ifdef ICARUS_VISITOR_EMIT_IR
#include "ast/dispatch_table.h"
#include "visitor/verify_type.h"
#endif // ICARUS_VISITOR_EMIT_IR

namespace ast {
struct Declaration;
struct Expression;
}  // namespace ast

namespace ir {
struct CompiledFn;
}  // namespace ir

struct DependentData {
  absl::flat_hash_map<ast::Declaration const *, ir::Reg> addr_;

  // TODO probably make these funcs constant.
  absl::node_hash_map<ast::Expression const *, ir::CompiledFn *> ir_funcs_;

#ifdef ICARUS_VISITOR_EMIT_IR
  // TODO future optimization: the bool determining if it's const is not
  // dependent and can therefore be stored more efficiently (though querying
  // for both simultaneously would be more expensive I guess.
  absl::flat_hash_map<ast::ExprPtr, visitor::VerifyResult> verify_results_;

// TODO this ifdef needs to disappear it's not long-term sustainable
  absl::flat_hash_map<ast::ExprPtr, ast::DispatchTable> dispatch_tables_;
#endif
  ConstantBinding constants_;
};

#endif  // ICARUS_MISC_DEPENDENT_DATA_H
