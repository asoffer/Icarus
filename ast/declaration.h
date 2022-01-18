#ifndef ICARUS_AST_DECLARATION_H
#define ICARUS_AST_DECLARATION_H

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "absl/types/span.h"
#include "ast/expression.h"
#include "ast/node.h"
#include "frontend/source/buffer.h"

namespace ast {

struct Declaration;

// Refers to a single identifier being declared within a declaration.
//
// TODO: Does it make sense for this to be an expression?
struct Declaration_Id : Expression {
  explicit Declaration_Id(std::string name, frontend::SourceRange const &range)
      : Expression(IndexOf<Declaration_Id>(), range), name_(std::move(name)) {}

  std::string_view name() const { return name_; }
  ast::Declaration const &declaration() const {
    return *ASSERT_NOT_NULL(decl_);
  }

  std::pair<std::string, frontend::SourceRange> extract() && {
    return std::pair<std::string, frontend::SourceRange>(std::move(name_),
                                                         std::move(range_));
  }

  void DebugStrAppend(std::string *out, size_t) const override {
    out->append(name());
  }
  void Initialize(Initializer &initializer) override {
    scope_ = initializer.scope;
  }

 private:
  friend struct Declaration;
  void set_decl(Declaration const *decl) { decl_ = decl; }

  ast::Declaration const *decl_;
  std::string name_;
};

// Declaration:
//
// Represents a declaration of a new identifier. The declaration may be for a
// local variable, global variable, function input parameter or return
// parameter. It may be const or metable.
//
// Examples:
//  * `a: i32`
//  * `b: bool = true`
//  * `c := 17`
//  * `d: f32 = --`
//  * `DAYS_PER_WEEK :: i32 = 7`
//  * `HOURS_PER_DAY ::= 24`
//  * `some_constant :: bool` ... This approach only makes sense when
//                                `some_constant` is a (generic) function
//                                parmaeter
//  * (a, b) := function_returning_two_values()
//
struct Declaration : Expression {
  using Flags                           = uint8_t;
  static constexpr Flags f_IsFnParam    = 0x01;
  static constexpr Flags f_IsOutput     = 0x02;
  static constexpr Flags f_IsConst      = 0x04;
  static constexpr Flags f_InitIsHole   = 0x08;
  static constexpr Flags f_IsBlockParam = 0x10;

  using Id = Declaration_Id;

  explicit Declaration(frontend::SourceRange const &range, std::vector<Id> ids,
                       std::unique_ptr<Expression> type_expression,
                       std::unique_ptr<Expression> initial_val, Flags flags)
      : Expression(IndexOf<Declaration>(), range),
        ids_(std::move(ids)),
        type_expr_(std::move(type_expression)),
        init_val_(std::move(initial_val)),
        flags_(flags) {
    for (auto &id : ids_) { id.set_decl(this); }
  }

  Declaration(Declaration &&decl) noexcept
      : Expression(IndexOf<Declaration>(), decl.range()),
        ids_(std::move(decl.ids_)),
        type_expr_(std::move(decl.type_expr_)),
        init_val_(std::move(decl.init_val_)),
        flags_(decl.flags_) {
    for (auto &id : ids_) { id.set_decl(this); }
    hashtags = std::move(decl.hashtags);
  }
  Declaration &operator=(Declaration &&decl) noexcept {
    ids_       = std::move(decl.ids_);
    type_expr_ = std::move(decl.type_expr_);
    init_val_  = std::move(decl.init_val_);
    flags_     = decl.flags_;
    hashtags   = std::move(decl.hashtags);
    for (auto &id : ids_) { id.set_decl(this); }
    return *this;
  }

  // TODO: These functions are confusingly named. They look correct in normal
  // declarations, but in function arguments, IsDefaultInitialized() is true
  // iff there is no default value provided.
  bool IsDefaultInitialized() const {
    return not init_val_ and not IsUninitialized();
  }
  bool IsUninitialized() const { return (flags_ & f_InitIsHole); }

  enum Kind {
    kDefaultInit              = 0,
    kCustomInit               = 2,
    kInferred                 = 3,
    kUninitialized            = 6,
    kInferredAndUninitialized = 7,  // This is an error
    kBinding                  = 9,
  };
  Kind kind() const {
    int k = type_expr() ? 0 : 1;
    if (IsUninitialized()) { k |= kUninitialized; }
    if (initial_value()) { k |= kCustomInit; }
    if (covers_binding()) { k |= kBinding; }
    return static_cast<Kind>(k);
  }

  absl::Span<Id const> ids() const { return ids_; }

  Expression const *type_expr() const { return type_expr_.get(); }
  Expression const *init_val() const { return init_val_.get(); }
  Expression const *initial_value() const { return init_val_.get(); }

  std::tuple<std::vector<Id>, std::unique_ptr<Expression>,
             std::unique_ptr<Expression>>
  extract() && {
    return std::make_tuple(std::move(ids_), std::move(type_expr_),
                           std::move(init_val_));
  }

  Flags flags() const { return flags_; }
  Flags &flags() { return flags_; }  // TODO consider removing this.

  void set_initial_value(std::unique_ptr<Expression> expr);

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Initializer &initializer) override;

 private:
  std::vector<Id> ids_;
  std::unique_ptr<Expression> type_expr_, init_val_;
  Flags flags_;
};

}  // namespace ast

#endif  // ICARUS_AST_DECLARATION_H
