#ifndef ICARUS_AST_DISPATCH_H
#define ICARUS_AST_DISPATCH_H

#include <optional>
#include <string>
#include <variant>
#include "base/container/unordered_map.h"

#include "base/container/map.h"
#include "fn_args.h"

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

  Binding(AST::Expression *fn_expr, type::Function const *fn_type, size_t n)
      : fn_expr_(fn_expr),
        fn_type_(fn_type),
        exprs_(n, std::pair<type::Type *, Expression *>(nullptr, nullptr)) {}

  Expression *fn_expr_           = nullptr;
  type::Function const *fn_type_ = nullptr;
  base::vector<std::pair<const type::Type *, Expression *>> exprs_;
};

// Represents a row in the dispatch table.
struct DispatchEntry {
  bool SetTypes(FunctionLiteral *fn, type::Function const *fn_type,
                Context *ctx);

  static std::optional<DispatchEntry> Make(Expression *fn_option,
                                           type::Function const *fn_option_type,
                                           const FnArgs<Expression *> &args,
                                           Context *ctx);

  FnArgs<const type::Type *> call_arg_types_;
  Binding binding_;

 private:
  DispatchEntry(Binding b) : binding_(std::move(b)) {}
};

struct DispatchTable {
  // TODO come up with a good internal representaion.
  // * Can/should this be balanced to find the right type-check sequence in a
  //   streaming manner?
  // * Add weights for PGO optimizations?

  static std::pair<DispatchTable, const type::Type *> Make(
      const FnArgs<Expression *> &args, Expression *fn, Context *ctx);
  static std::pair<DispatchTable, const type::Type *> Make(
      const FnArgs<Expression *> &args, const std::string &op, Scope *scope,
      Context *ctx);

  void InsertEntry(DispatchEntry entry);

  base::map<FnArgs<const type::Type *>, Binding> bindings_;
  size_t total_size_ = 0;
};

}  // namespace AST

#endif  // ICARUS_AST_DISPATCH_H
