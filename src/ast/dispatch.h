#ifndef ICARUS_AST_DISPATCH_H
#define ICARUS_AST_DISPATCH_H

#include <string>
#include <variant>

#include "ast/bound_constants.h"
#include "ast/fn_args.h"
#include "ast/overload_set.h"
#include "base/container/map.h"
#include "base/container/unordered_map.h"
#include "base/expected.h"

struct Context;
struct Scope;

namespace type {
struct Type;
struct Function;
}  // namespace type

namespace ast {
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
  struct Entry {
    Entry() = default;
    Entry(Expression *expr, int argument_index, int expansion_index,
          size_t parameter_index)
        : expr(expr),
          argument_index(argument_index),
          expansion_index(expansion_index),
          parameter_index(parameter_index) {}

    constexpr bool defaulted() const { return expr == nullptr; }

    Expression *expr = nullptr;
    int argument_index = -1;  // Positive numbers indicate positional arguments.
                              // -1 indicates named argument.
    int expansion_index = -1;     // Positive numbers indicate the index of an
                                  // expansion. -1 indicates no expansion
                                  // necessary.
    size_t parameter_index = -1;  // Which parameter this gets associated with.
    type::Type const *type = nullptr;
  };

  // Returns true if on this binding no argument is provided for the parameter
  // at index `i`.
  bool defaulted(size_t i) const;

  Binding(type::Typed<Expression *, type::Callable> fn, bool constant = false)
      : fn_(fn), const_(constant) {}

  type::Typed<Expression *, type::Callable> fn_;
  base::vector<Entry> entries_;

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
      FnArgs<Expression *> const &args, OverloadSet const &overload_set,
      Context *ctx);

  base::vector<ir::Val> EmitCall(
      ast::FnArgs<std::pair<ast::Expression *, base::vector<ir::Val>>> const
          &args,
      type::Type const *ret_type, Context *ctx) const;

  base::map<FnArgs<type::Type const *>, Binding> bindings_;
  std::unordered_map<Expression const *, std::string> failure_reasons_;
  size_t total_size_ = 0;
};

}  // namespace ast

#endif  // ICARUS_AST_DISPATCH_H
