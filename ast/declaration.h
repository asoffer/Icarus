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
//
struct Declaration : Expression {
  using Flags                           = uint8_t;
  static constexpr Flags f_IsFnParam    = 0x01;
  static constexpr Flags f_IsOutput     = 0x02;
  static constexpr Flags f_IsConst      = 0x04;
  static constexpr Flags f_InitIsHole   = 0x08;
  static constexpr Flags f_IsBlockParam = 0x10;

  explicit Declaration(frontend::SourceRange const &range,
                       std::vector<std::string> ids,
                       std::vector<frontend::SourceRange> id_ranges,
                       std::unique_ptr<Expression> type_expression,
                       std::unique_ptr<Expression> initial_val, Flags flags)
      : Expression(range),
        ids_(std::move(ids)),
        id_ranges_(std::move(id_ranges)),
        type_expr_(std::move(type_expression)),
        init_val_(std::move(initial_val)),
        flags_(flags) {}
  Declaration(Declaration &&) noexcept = default;
  Declaration &operator=(Declaration &&) noexcept = default;

  struct const_iterator {
    const_iterator operator++() {
      ++index_;
      return *this;
    }
    const_iterator operator++(int) {
      auto result = *this;
      ++index_;
      return result;
    }

    Declaration const &declaration() const { return *decl_; }
    std::string_view id() const { return decl_->ids()[index_]; }
    frontend::SourceRange const &id_range() const {
      return decl_->id_ranges()[index_];
    }

    friend constexpr bool operator==(const_iterator lhs, const_iterator rhs) {
      return lhs.index_ == rhs.index_ and lhs.decl_ == rhs.decl_;
    }
    friend constexpr bool operator!=(const_iterator lhs, const_iterator rhs) {
      return not(lhs == rhs);
    }
    const_iterator operator*() const { return *this; }
    const_iterator const *operator->() const { return this; }

   private:
    friend struct Declaration;
    explicit constexpr const_iterator(size_t index, Declaration const *decl)
        : index_(index), decl_(decl) {}

    size_t index_;
    Declaration const *decl_;
  };

  const_iterator begin() const { return const_iterator(0, this); }
  const_iterator end() const { return const_iterator(size(), this); }

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
  };
  Kind kind() const {
    int k = type_expr() ? 0 : 1;
    if (IsUninitialized()) { k |= kUninitialized; }
    if (initial_value()) { k |= kCustomInit; }
    return static_cast<Kind>(k);
  }

  size_t size() const { return ids_.size(); }

  absl::Span<std::string const> ids() const { return ids_; }
  absl::Span<frontend::SourceRange const> id_ranges() const {
    return id_ranges_;
  }
  Expression const *type_expr() const { return type_expr_.get(); }
  Expression const *init_val() const { return init_val_.get(); }
  Expression const *initial_value() const { return init_val_.get(); }

  std::tuple<std::vector<std::string>, std::unique_ptr<Expression>,
             std::unique_ptr<Expression>>
  extract() && {
    return std::make_tuple(std::move(ids_), std::move(type_expr_),
                           std::move(init_val_));
  }

  Flags flags() const { return flags_; }
  Flags &flags() { return flags_; }  // TODO consider removing this.

  void set_initial_value(std::unique_ptr<Expression> expr);

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Scope *scope) override;
  bool IsDependent() const override;

 private:
  std::vector<std::string> ids_;
  std::vector<frontend::SourceRange> id_ranges_;
  std::unique_ptr<Expression> type_expr_, init_val_;
  Flags flags_;
};

}  // namespace ast

#endif  // ICARUS_AST_DECLARATION_H
