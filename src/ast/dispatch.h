#ifndef ICARUS_AST_DISPATCH_H
#define ICARUS_AST_DISPATCH_H

#include <optional>
#include <string>
#include <variant>
#include "base/container/unordered_map.h"

#include "ast/bound_constants.h"
#include "ast/fn_args.h"
#include "ast/overload_set.h"
#include "base/container/map.h"

struct Context;
struct Scope;

namespace type {
struct Type;
struct Function;
}  // namespace type

namespace AST {
struct FunctionLiteral;
struct Expression;
// Represents a particular call resolution.
struct Binding {
  void SetPositionalArgs(const FnArgs<Expression *> &args);
  bool SetNamedArgs(
      const FnArgs<Expression *> &args,
      const base::unordered_map<std::string, size_t> &index_lookup);

  bool defaulted(size_t i) const { return exprs_[i].second == nullptr; }

  Binding(type::Typed<Expression *, type::Function> fn, size_t n)
      : fn_(fn),
        exprs_(n, std::pair<type::Type *, Expression *>(nullptr, nullptr)) {}

  type::Typed<Expression *, type::Function> fn_;
  base::vector<std::pair<const type::Type *, Expression *>> exprs_;
};

struct DispatchTable {
  // TODO come up with a good internal representaion.
  // * Can/should this be balanced to find the right type-check sequence in a
  //   streaming manner?
  // * Add weights for PGO optimizations?

  static std::pair<DispatchTable, type::Type const *> Make(
      FnArgs<Expression *> const &args, OverloadSet const &overload_set,
      Context *ctx);

  base::map<FnArgs<const type::Type *>, Binding> bindings_;
  size_t total_size_ = 0;
};

}  // namespace AST

#endif  // ICARUS_AST_DISPATCH_H
