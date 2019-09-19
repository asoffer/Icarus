#ifndef ICARUS_AST_DISPATCH_TABLE_H
#define ICARUS_AST_DISPATCH_TABLE_H

#include <string>
#include <variant>

#include "absl/container/flat_hash_map.h"
#include "core/fn_args.h"
#include "core/fn_params.h"
#include "ir/basic_block.h"
#include "ir/block.h"
#include "ir/reg.h"
#include "ir/results.h"
#include "type/typed_value.h"
#include "visitor/verify_result.h"

struct Context;

namespace visitor {
struct TraditionalCompilation;
}  // namespace visitor

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

  static std::pair<DispatchTable, type::Type const *> Make(
      core::FnArgs<type::Typed<Expression const *>> const &args,
      OverloadSet const &overload_set, Context *ctx);

  ir::Results EmitInlineCall(
      visitor::TraditionalCompilation *visitor,
      core::FnArgs<std::pair<Expression const *, ir::Results>> const &args,
      absl::flat_hash_map<ir::BlockDef const *, ir::BasicBlock *> const
          &block_map) const;
  ir::Results EmitCall(
      visitor::TraditionalCompilation *visitor,
      core::FnArgs<std::pair<Expression const *, ir::Results>> const &args,
      bool is_inline = false) const;

  std::vector<Row> bindings_;
  std::vector<type::Type const *> return_types_;
};

visitor::VerifyResult VerifyJumpDispatch(
    visitor::TraditionalCompilation *visitor, ExprPtr expr,
    absl::Span<ir::AnyFunc const> overload_set,
    core::FnArgs<std::pair<Expression const *, visitor::VerifyResult>> const
        &args,
    std::vector<ir::BlockDef const *> *block_defs);

visitor::VerifyResult VerifyDispatch(
    visitor::TraditionalCompilation *visitor, ExprPtr expr,
    absl::Span<ir::AnyFunc const> overload_set,
    core::FnArgs<std::pair<Expression const *, visitor::VerifyResult>> const
        &args);

visitor::VerifyResult VerifyDispatch(
    visitor::TraditionalCompilation *visitor, ExprPtr expr,
    OverloadSet const &overload_set,
    core::FnArgs<std::pair<Expression const *, visitor::VerifyResult>> const
        &args);

}  // namespace ast

#endif  // ICARUS_AST_DISPATCH_TABLE_H
