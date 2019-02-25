#ifndef ICARUS_AST_DISPATCH_TABLE_H
#define ICARUS_AST_DISPATCH_TABLE_H

#include <string>
#include <variant>

#include <unordered_map>
#include "ast/bound_constants.h"
#include "ast/dispatch/arg_resolution.h"
#include "ast/fn_args.h"
#include "ast/overload_set.h"
#include "base/expected.h"

struct Context;
struct Scope;

namespace type {
struct Type;
struct Function;
}  // namespace type

namespace ast {
struct Node;
struct FunctionLiteral;
struct Expression;

// Represents a particular call resolution. This means the precise callable
// and arguments along with their types. the associated types may differ from
// the type of the callable or arguments computed by VerifyType. This is
// because, an argument which is a variant may be dispatched to more than one
// place. So, for instance if there are functions `id :: bool -> bool` and
// `id ::= int -> int`, then calling `id` with a variant will produce two
// bindings: One for bool and one for int. Simlarly, the type of `id` on it's
// own is expressed as an overload set, but for each particular binding will be
// either `int -> int` or `bool -> bool`.
struct Binding {
  Binding(type::Typed<Expression *, type::Callable> fn, bool constant = false)
      : fn_(fn), const_(constant) {}

  type::Typed<Expression *, type::Callable> fn_;
  ArgResolution arg_res_;

  bool const_ = false;
  BoundConstants
      bound_constants_;  // TODO don't copy these. Use some sitting on a module.
};

struct DispatchTable {
  // TODO come up with a good internal representaion.
  // * Can/should this be balanced to find the right type-check sequence in a
  //   streaming manner?
  // * Add weights for PGO optimizations?

  static std::pair<DispatchTable, type::Type const *> Make(
      FnArgs<type::Typed<Expression *>> const &args,
      OverloadSet const &overload_set, Context *ctx);
  static type::Type const *MakeOrLogError(Node *node,
                                          FnArgs<Expression *> const &args,
                                          OverloadSet const &overload_set,
                                          Context *ctx, bool repeated = false);

  ir::Results EmitCall(
      ast::FnArgs<std::pair<ast::Expression *, ir::Results>> const &args,
      type::Type const *ret_type, Context *ctx) const;

  std::map<FnArgs<type::Type const *>, Binding> bindings_;
  std::unordered_map<Expression const *, std::string> failure_reasons_;
  std::vector<std::string> generic_failure_reasons_;
};

}  // namespace ast

#endif  // ICARUS_AST_DISPATCH_TABLE_H
