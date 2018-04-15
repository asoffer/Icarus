#ifndef ICARUS_AST_DISPATCH_H
#define ICARUS_AST_DISPATCH_H

#include <map>
#include <string>
#include <unordered_map>
#include <optional>
#include <variant>

#include "fn_args.h"

namespace type {
struct Type;
} // namespace type

namespace AST {
struct Expression;

// Represents a particular call resolution.
struct Binding {
  static std::optional<Binding>
  MakeUntyped(Expression *fn_expr, const FnArgs<Expression *> &args,
              const std::unordered_map<std::string, size_t> &index_lookup);

  bool defaulted(size_t i) const { return exprs_[i].second == nullptr; }

  Binding() = default;
  Expression *fn_expr_ = nullptr;
  std::vector<std::pair<const type::Type *, Expression *>> exprs_;

private:
  Binding(Expression *fn_expr, size_t n);
};

struct DispatchTable {
  // TODO come up with a good internal representaion.
  // * Can/should this be balanced to find the right type-check sequence in a
  //   streaming manner?
  // * Add weights for PGO optimizations?

  void insert(FnArgs<const type::Type *> call_arg_types, Binding binding,
              size_t expanded_size = std::numeric_limits<size_t>::max());

  std::map<FnArgs<const type::Type *>, Binding> bindings_;
  size_t total_size_ = 0;
};
} // namespace AST

#endif // ICARUS_AST_DISPATCH_H
