#ifndef ICARUS_AST_AST_H
#define ICARUS_AST_AST_H

#include <memory>
#include <string>

#include "absl/types/span.h"
#include "ast/expression.h"
#include "frontend/operators.h"

namespace ast {

// Access:
// Represents member access with the `.` operator.
//
// Examples:
//  * `my_pair.first_element` 
//  * `(some + computation).member`
struct Access : public Expression {
  Access(TextSpan span, std::unique_ptr<Expression> operand,
         std::string member_name)
      : Expression(std::move(span)),
        operand_(std::move(operand)),
        member_name_(std::move(member_name)) {}
  ~Access() override {}

  std::string_view member_name() const { return member_name_; }
  Expression const *operand() const { return operand_.get(); }
  Expression *operand() { return operand_.get(); }

#include "visitor/visitors.xmacro.h"

 private:
  std::unique_ptr<Expression> operand_;
  std::string member_name_;
};

// ArrayLiteral:
// Represents a literal array, which can have any number of elements (including
// zero).
//
// Examples:
//  * `[1, 2, 3, 5, 8]`
//  * `[thing1, thing2]`
//  * `[im_the_only_thing]`
//  * `[]`
struct ArrayLiteral : public Expression {
  ArrayLiteral(TextSpan span, std::unique_ptr<Expression> expr)
      : Expression(std::move(span)) {
    exprs_.push_back(std::move(expr));
  }
  ArrayLiteral(TextSpan span, std::vector<std::unique_ptr<Expression>> exprs)
      : Expression(std::move(span)), exprs_(std::move(exprs)) {}
  ~ArrayLiteral() override {}

  bool empty() const { return exprs_.empty(); }
  size_t size() const { return exprs_.size(); }
  Expression const *elem(size_t i) const { return exprs_[i].get(); }
  // TODO hide the unique_ptr here.
  absl::Span<std::unique_ptr<Expression> const> elems() const { return exprs_; }

#include "visitor/visitors.xmacro.h"

 private:
  std::vector<std::unique_ptr<Expression>> exprs_;
};

// ArrayType:
// Represents the syntactic construction for expressing the type of an array.
// The relevant data for describing the full type of an array is its length and
// the type of the contained elements. Each of these are expressed as
// expressions.
//
// Examples:
//  * `[5; int32]`      -- Represnts the type of an array that can hold five
//                         32-bit integers
//  * `[0; bool]`       -- Represents the type of an array that can hold zero
//                         booleans.
//  * `[3; [2; int8]]`  -- Represents the type of an array that can hold three
//                         elements, each of which is an array that can hold two
//                         8-bit integers.
//  * `[3, 2; int8]`    -- A shorthand syntax for `[3; [2; int8]]`
//
//  TODO: Currently when we build an array type, even when we syntactically see
//  `[3, 2; int8]` we build the tree as if it were `[3; [2; int8]]`. This is
//  hostile to formatters that want to traverse the tree and keep syntax similar
//  to what the original author wrote.
struct ArrayType : public Expression {
  ArrayType(TextSpan span, std::unique_ptr<Expression> length,
            std::unique_ptr<Expression> data_type)
      : Expression(std::move(span)),
        length_(std::move(length)),
        data_type_(std::move(data_type)) {}
  ~ArrayType() override {}

  Expression const *length() const { return length_.get(); }
  Expression *length() { return length_.get(); }

  Expression const *data_type() const { return data_type_.get(); }
  Expression *data_type() { return data_type_.get(); }

#include "visitor/visitors.xmacro.h"

 private:
  std::unique_ptr<Expression> length_, data_type_;
};

// Binop:
// Represents a call to a binary operator.
//
// Examples:
//  * `thing1 + thing2`
//  * `3 * (x + y)`
//
// Note that some things one might expect to be binary operators are treated
// differently (see `ChainOp`). This is because in Icarus, operators such as
// `==` allow chains so that `x == y == z` can evaluate to `true` if and only if
// both `x == y` and `y == z`.
struct Binop : public Expression {
  Binop(std::unique_ptr<Expression> lhs, frontend::Operator op,
        std::unique_ptr<Expression> rhs)
      : Expression(TextSpan(lhs->span, rhs->span)),
        op_(op),
        lhs_(std::move(lhs)),
        rhs_(std::move(rhs)) {}
  ~Binop() override {}

  Expression const* lhs() const { return lhs_.get(); }
  Expression* lhs() { return lhs_.get(); }
  Expression const* rhs() const { return rhs_.get(); }
  Expression* rhs() { return rhs_.get(); }
  frontend::Operator op() const { return op_; }

  std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>> extract()
      && {
    return std::pair{std::move(lhs_), std::move(rhs_)};
  }

#include "visitor/visitors.xmacro.h"

  frontend::Operator op_;
  std::unique_ptr<Expression> lhs_, rhs_;
};

}  // namespace ast

#include "ast/block_literal.h"
#include "ast/block_node.h"
#include "ast/builtin_fn.h"
#include "ast/call.h"
#include "ast/cast.h"
#include "ast/chainop.h"
#include "ast/comma_list.h"
#include "ast/declaration.h"
#include "ast/enum_literal.h"
#include "ast/expression.h"
#include "ast/function_literal.h"
#include "ast/hashtag.h"
#include "ast/identifier.h"
#include "ast/import.h"
#include "ast/index.h"
#include "ast/interface.h"
#include "ast/match_declaration.h"
#include "ast/node.h"
#include "ast/repeated_unop.h"
#include "ast/scope_literal.h"
#include "ast/scope_node.h"
#include "ast/statements.h"
#include "ast/struct_literal.h"
#include "ast/struct_type.h"
#include "ast/switch.h"
#include "ast/terminal.h"
#include "ast/unop.h"

#endif  // ICARUS_AST_AST_H
