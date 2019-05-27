#ifndef ICARUS_AST_AST_H
#define ICARUS_AST_AST_H

#include <memory>
#include <string>
#include <type_traits>
#include <vector>

#include "absl/types/span.h"
#include "ast/declaration.h"
#include "ast/expression.h"
#include "ast/node_span.h"
#include "ast/statements.h"
#include "core/builtin.h"
#include "core/fn_args.h"
#include "core/scope.h"
#include "frontend/operators.h"

namespace ast {

template <typename S>
struct ScopeExpr : public Expression {
  ScopeExpr(TextSpan &&span) : Expression(std::move(span)) {}
  ~ScopeExpr() override {}
  ScopeExpr(ScopeExpr &&) noexcept = default;
  ScopeExpr &operator=(ScopeExpr &&) noexcept = default;

  void set_body_with_parent(core::Scope *p) {
    body_scope_ = p->template add_child<S>();
  }
  S *body_scope() { return body_scope_.get(); }
  S const *body_scope() const { return body_scope_.get(); }

 private:
  std::unique_ptr<S> body_scope_ = nullptr;
};

// Access:
// Represents member access with the `.` operator.
//
// Examples:
//  * `my_pair.first_element` 
//  * `(some + computation).member`
struct Access : public Expression {
  explicit Access(TextSpan span, std::unique_ptr<Expression> operand,
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
  explicit ArrayLiteral(TextSpan span, std::unique_ptr<Expression> expr)
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
  NodeSpan<Expression const> elems() const { return exprs_; }
  NodeSpan<Expression> elems() { return exprs_; }

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
struct ArrayType : public Expression {
  explicit ArrayType(TextSpan span, std::unique_ptr<Expression> length,
                     std::unique_ptr<Expression> data_type)
      : Expression(std::move(span)), data_type_(std::move(data_type)) {
    lengths_.push_back(std::move(length));
  }
  ArrayType(TextSpan span, std::vector<std::unique_ptr<Expression>> lengths,
            std::unique_ptr<Expression> data_type)
      : Expression(std::move(span)),
        lengths_(std::move(lengths)),
        data_type_(std::move(data_type)) {}
  ~ArrayType() override {}

  NodeSpan<Expression const> lengths() const { return lengths_; }
  NodeSpan<Expression> lengths() { return lengths_; }
  Expression *length(size_t i) { return lengths_[i].get(); }
  Expression const *length(size_t i) const { return lengths_[i].get(); }

  Expression const *data_type() const { return data_type_.get(); }
  Expression *data_type() { return data_type_.get(); }

#include "visitor/visitors.xmacro.h"

 private:
  std::vector<std::unique_ptr<Expression>> lengths_;
  std::unique_ptr<Expression> data_type_;
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
  explicit Binop(std::unique_ptr<Expression> lhs, frontend::Operator op,
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

 private:
  frontend::Operator op_;
  std::unique_ptr<Expression> lhs_, rhs_;
};

// BlockLiteral:
//
// Represents the specification for a block in a scope. The `before`
// declarations constitute the objects assigned to the identifier `before`.
// These constitute the overload set for functions to be called before entering
// the corresponding block. Analogously, The declarations in `after` constitute
// the overload set for functions to be called after exiting the block.
//
// Example:
//  ```
//  block {
//    before ::= () -> () {}
//    after  ::= () -> () { jump exit() }
//  }
//  ```
struct BlockLiteral : public ScopeExpr<core::DeclScope> {
  explicit BlockLiteral(TextSpan span,
                        std::vector<std::unique_ptr<Declaration>> before,
                        std::vector<std::unique_ptr<Declaration>> after,
                        bool required)
      : ScopeExpr<core::DeclScope>(std::move(span)),
        before_(std::move(before)),
        after_(std::move(after)),
        required_(required) {}
  ~BlockLiteral() override {}

  NodeSpan<Declaration const> before() const { return before_; }
  NodeSpan<Declaration> before() { return before_; }
  NodeSpan<Declaration const> after() const { return after_; }
  NodeSpan<Declaration> after() { return after_; }

  bool is_required() const { return required_; }

#include "visitor/visitors.xmacro.h"

 private:
  std::vector<std::unique_ptr<Declaration>> before_, after_;
  bool required_;
};

// BlockNode:
//
// Represents a block in a scope at the usage-site (as opposed to where the
// author of the scope defined the block).
//
// Example:
//  ```
//  if (some_condition) then {
//    do_something()
//    do_another_thing()
//  } else {
//    do_something_else()
//  }
//  ```
//
//  In the code snippet above, `then { ... }` is a block with name "then" and
//  two statements. `else { ... }` is another block with name "else" and one
//  statement.
//
// Note: Today blocks have names and statements but cannot take any arguments.
// This will likely change in the future so that blocks can take arguments
// (likely in the form of `core::FnArgs<std::unique_ptr<ast::Expression>>`).
struct BlockNode : public ScopeExpr<core::ExecScope> {
  explicit BlockNode(TextSpan span, std::string name,
                     std::vector<std::unique_ptr<Node>> stmts)
      : ScopeExpr<core::ExecScope>(std::move(span)),
        name_(std::move(name)),
        stmts_(std::move(stmts)) {}
  ~BlockNode() override {}
  BlockNode(BlockNode &&) noexcept = default;
  BlockNode &operator=(BlockNode &&) noexcept = default;

  std::string_view name() const { return name_; }
  NodeSpan<Node> stmts() { return stmts_; }
  NodeSpan<Node const> stmts() const { return stmts_; }

#include "visitor/visitors.xmacro.h"

 private:
  std::string name_;
  std::vector<std::unique_ptr<Node>> stmts_;
};

// Represents a builtin (possibly generic) function. Examples include `foreign`,
// which declares a foreign-function by name, or `opaque` which constructs a new
// type with no known size or alignment (users can pass around pointers to
// values of an opaque type, but not actual values).
struct BuiltinFn : public Expression {
  explicit BuiltinFn(TextSpan span, core::Builtin b)
      : Expression(std::move(span)), val_(b) {}
  ~BuiltinFn() override {}

  core::Builtin value() const { return val_; }

#include "visitor/visitors.xmacro.h"

 private:
  core::Builtin val_;
};

}  // namespace ast

#include "ast/call.h"
#include "ast/cast.h"
#include "ast/chainop.h"
#include "ast/comma_list.h"
#include "ast/enum_literal.h"
#include "ast/function_literal.h"
#include "ast/hashtag.h"
#include "ast/identifier.h"
#include "ast/import.h"
#include "ast/index.h"
#include "ast/interface.h"
#include "ast/match_declaration.h"
#include "ast/node.h"

namespace ast {

// RepeatedUnop:
// Represents a statement where arbitrarily many expressions can be passed, and
// are all treated as arguments to the same unary operator (for a very loose
// definition of operator).
//
// Example:
//  ```
//  print "hello", 42
//  return 3, 4, 5
//  ```
//
//  Both `print` and `return` are repeated unary operators.
//
//  Note: This node would probably be better expressed as a JumpNode and a
//  PrintNode separately. And perhaps get rid of print altogether because you
//  can use the foreign funtion `puts`. Also, right now import nodes can't be
//  repeated, but maybe they should be?
struct RepeatedUnop : public Node {
  explicit RepeatedUnop(TextSpan span, frontend::Operator op,
                        std::vector<std::unique_ptr<Expression>> exprs)
      : Node(std::move(span)), op_(op), exprs_(std::move(exprs)) {}
  ~RepeatedUnop() override {}

  frontend::Operator op() const { return op_; }
  NodeSpan<Expression> exprs() { return exprs_; }
  NodeSpan<Expression const> exprs() const { return exprs_; }
  Expression const *expr(size_t i) const { return exprs_[i].get(); }

#include "visitor/visitors.xmacro.h"

 private:
  frontend::Operator op_;
  std::vector<std::unique_ptr<Expression>> exprs_;
};

}  // namespace ast

#include "ast/scope_literal.h"
namespace ast {

struct ScopeNode : public Expression {
  ~ScopeNode() override {}

#include "visitor/visitors.xmacro.h"

  std::unique_ptr<Expression> name_;
  core::FnArgs<std::unique_ptr<Expression>> args_;
  std::vector<BlockNode> blocks_;
  ScopeNode *sugared_ = nullptr;
};

}  // namespace ast

#include "ast/statements.h"
#include "ast/struct_literal.h"
#include "ast/struct_type.h"
#include "ast/switch.h"
#include "ast/terminal.h"
#include "ast/unop.h"

#endif  // ICARUS_AST_AST_H
