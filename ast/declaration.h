#ifndef ICARUS_AST_DECLARATION_H
#define ICARUS_AST_DECLARATION_H

#include "ast/expression.h"
#include "ir/register.h"

struct Module;
namespace ir {
struct Val;
}  // namespace ir

namespace ast {
struct Declaration : public Expression {
  Declaration(bool is_const = false) : const_(is_const) {}
  Declaration(Declaration &&) noexcept = default;
  Declaration &operator=(Declaration &&) noexcept = default;
  ~Declaration() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    std::stringstream ss;
    ss << id_;
    if (type_expr) {
      ss << (const_ ? " :: " : ": ") << type_expr->to_string(n);
      if (init_val) { ss << " = " << init_val->to_string(n); }
    } else {
      if (init_val) {
        ss << (const_ ? " ::= " : " := ") << init_val->to_string(n);
      }
    }

    return ss.str();
  }

  std::string id_;
  std::unique_ptr<Expression> type_expr, init_val;

  Module *mod_ = nullptr;

  // Field in a function, whether or not it's an input our output.
  bool is_fn_param_  = false;
  bool is_output_    = false;
  bool const_        = false;
  bool init_is_hole_ = false;

  // These functions are confusingly named. They look correct in normal
  // declarations, but in function arguments, IsDefaultInitialized() is true iff
  // there is no default value provided.
  bool IsInferred() const { return !type_expr; }
  bool IsDefaultInitialized() const { return !init_val && !init_is_hole_; }
  bool IsCustomInitialized() const { return init_val.get(); }
  bool IsUninitialized() const { return init_is_hole_; }
};
}  // namespace ast

#endif  // ICARUS_AST_DECLARATION_H
