#ifndef ICARUS_AST_DISPATCH_TABLE_H
#define ICARUS_AST_DISPATCH_TABLE_H

#include <string>
#include <variant>

#include "absl/container/flat_hash_map.h"
#include "compiler/verify_result.h"
#include "core/fn_args.h"
#include "core/fn_params.h"
#include "ir/basic_block.h"
#include "ir/block_def.h"
#include "ir/reg.h"
#include "ir/results.h"
#include "type/typed_value.h"

namespace compiler {
struct Compiler;
}  // namespace compiler

namespace type {
struct Type;
struct Function;
}  // namespace type

namespace ast {
struct OverloadSet;
struct Node;
struct Expression;
struct ExprPtr;

struct DispatchTable {
  struct Row {
    Row(core::FnParams<type::Typed<Expression const *>> p,
        type::Function const *t,
        std::variant<Expression const *, ir::AnyFunc> f)
        : params(std::move(p)), type(t), fn(std::move(f)) {}

    // In the typed-expression, each expression may be null (if no default value
    // is possible), but the type will always be present.
    core::FnParams<type::Typed<Expression const *>> params;
    type::Function const *type;
    std::variant<Expression const *, ir::AnyFunc> fn;
  };

  ir::Results EmitInlineCall(
      compiler::Compiler *visitor,
      core::FnArgs<std::pair<Expression const *, ir::Results>> const &args,
      absl::flat_hash_map<ir::BlockDef const *, ir::BasicBlock *> const
          &block_map) const;
  ir::Results EmitCall(
      compiler::Compiler *visitor,
      core::FnArgs<std::pair<Expression const *, ir::Results>> const &args,
      bool is_inline = false) const;

  std::vector<Row> bindings_;
  std::vector<type::Type const *> return_types_;
};

compiler::VerifyResult VerifyJumpDispatch(
    compiler::Compiler *visitor, ExprPtr expr,
    absl::Span<ir::AnyFunc const> overload_set,
    core::FnArgs<std::pair<Expression const *, compiler::VerifyResult>> const
        &args,
    std::vector<ir::BlockDef const *> *block_defs);

compiler::VerifyResult VerifyDispatch(
    compiler::Compiler *visitor, ExprPtr expr,
    absl::Span<ir::AnyFunc const> overload_set,
    core::FnArgs<std::pair<Expression const *, compiler::VerifyResult>> const
        &args);

compiler::VerifyResult VerifyDispatch(
    compiler::Compiler *visitor, ExprPtr expr,
    OverloadSet const &overload_set,
    core::FnArgs<std::pair<Expression const *, compiler::VerifyResult>> const
        &args);

}  // namespace ast

#endif  // ICARUS_AST_DISPATCH_TABLE_H
