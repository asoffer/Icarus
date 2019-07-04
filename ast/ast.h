#ifndef ICARUS_AST_AST_H
#define ICARUS_AST_AST_H

#include <memory>
#include <string>
#include <type_traits>
#include <vector>

#include "absl/types/span.h"
#include "ast/expression.h"
#include "ast/hashtag.h"
#include "ast/node.h"
#include "ast/node_span.h"
#include "base/graph.h"
#include "core/builtin.h"
#include "core/fn_args.h"
#include "core/fn_params.h"
#include "core/ordered_fn_args.h"
#include "core/scope.h"
#include "frontend/operators.h"
#include "ir/block.h"
#include "ir/results.h"
#include "type/primitive.h"

namespace ast {

template <typename S>
struct ScopeExpr : public Expression {
  ScopeExpr(TextSpan &&span) : Expression(std::move(span)) {}
  ~ScopeExpr() override {}
  ScopeExpr(ScopeExpr &&) noexcept = default;
  ScopeExpr &operator=(ScopeExpr &&) noexcept = default;

  template <typename... Args>
  void set_body_with_parent(core::Scope *p, Args &&... args) {
    body_scope_ = p->template add_child<S>(std::forward<Args>(args)...);
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
  NodeSpan<Expression const> elems() const { return exprs_; }
  NodeSpan<Expression> elems() { return exprs_; }
  std::vector<std::unique_ptr<Expression>> &&extract() && {
    return std::move(exprs_);
  }

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
//  * `[5; int32]`     ... Represnts the type of an array that can hold five
//                         32-bit integers
//  * `[0; bool]`      ... Represents the type of an array that can hold zero
//                         booleans.
//  * `[3; [2; int8]]` ... Represents the type of an array that can hold three
//                         elements, each of which is an array that can hold two
//                         8-bit integers.
//  * `[3, 2; int8]`   ... A shorthand syntax for `[3; [2; int8]]`
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

// Declaration:
//
// Represents a declaration of a new identifier. The declaration may be for a
// local variable, global variable, function input parameter or return
// parameter. It may be const or metable.
//
// Examples:
//  * `a: int32`
//  * `b: bool = true`
//  * `c := 17`
//  * `d: float32 = --`
//  * `DAYS_PER_WEEK :: int32 = 7`
//  * `HOURS_PER_DAY ::= 24`
//  * `some_constant :: bool` ... This approach only makes sense when
//                                `some_constant` is a (generic) function
//                                parmaeter
//
struct Declaration : public Expression {
  using Flags                         = uint8_t;
  static constexpr Flags f_IsFnParam  = 0x01;
  static constexpr Flags f_IsOutput   = 0x02;
  static constexpr Flags f_IsConst    = 0x04;
  static constexpr Flags f_InitIsHole = 0x08;

  explicit Declaration(TextSpan span, std::string id,
                       std::unique_ptr<Expression> type_expression,
                       std::unique_ptr<Expression> initial_val, Flags flags)
      : Expression(std::move(span)),
        id_(std::move(id)),
        type_expr_(std::move(type_expression)),
        init_val_(std::move(initial_val)),
        flags_(flags) {}
  Declaration(Declaration &&) noexcept = default;
  Declaration &operator=(Declaration &&) noexcept = default;
  ~Declaration() override {}

  // TODO: These functions are confusingly named. They look correct in normal
  // declarations, but in function arguments, IsDefaultInitialized() is true iff
  // there is no default value provided.
  bool IsInferred() const { return !type_expr_; }
  bool IsDefaultInitialized() const { return !init_val_ && !IsUninitialized(); }
  bool IsCustomInitialized() const { return init_val_.get(); }
  bool IsUninitialized() const { return (flags_ & f_InitIsHole); }

  std::string_view id() const { return id_; }
  Expression const *type_expr() const { return type_expr_.get(); }
  Expression *type_expr() { return type_expr_.get(); }
  Expression const *init_val() const { return init_val_.get(); }
  Expression *init_val() { return init_val_.get(); }

  Flags flags() const { return flags_; }
  Flags &flags() { return flags_; }

  void set_initial_value(std::unique_ptr<Expression> expr) {
    init_val_ = std::move(expr);
  }

#include "visitor/visitors.xmacro.h"

 private:
  std::string id_;
  std::unique_ptr<Expression> type_expr_, init_val_;
  Flags flags_;
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
// Example (with no arguments):
//  ```
//  if (some_condition) then {
//    do_something()
//    do_another_thing()
//  } else {
//    do_something_else()
//  }
//  ```
//
// Example (with arguments):
//  ```
//  for_each (array_of_bools) do [elem: bool] {
//    if (elem) then { print "It's true!" } else { print "It's not so true." }
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
//
// TODO: `args` should be renamed to `params`.
struct BlockNode : public ScopeExpr<core::ExecScope> {
  explicit BlockNode(TextSpan span, std::string name,
                     std::vector<std::unique_ptr<Node>> stmts)
      : ScopeExpr<core::ExecScope>(std::move(span)),
        name_(std::move(name)),
        stmts_(std::move(stmts)) {}
  explicit BlockNode(TextSpan span, std::string name,
                     std::vector<std::unique_ptr<Expression>> args,
                     std::vector<std::unique_ptr<Node>> stmts)
      : ScopeExpr<core::ExecScope>(std::move(span)),
        name_(std::move(name)),
        args_(std::move(args)),
        stmts_(std::move(stmts)) {}
  ~BlockNode() override {}
  BlockNode(BlockNode &&) noexcept = default;
  BlockNode &operator=(BlockNode &&) noexcept = default;

  std::string_view name() const { return name_; }
  NodeSpan<Node> stmts() { return stmts_; }
  NodeSpan<Node const> stmts() const { return stmts_; }
  NodeSpan<Expression> args() { return args_; }
  NodeSpan<Expression const> args() const { return args_; }

#include "visitor/visitors.xmacro.h"

 private:
  std::string name_;
  std::vector<std::unique_ptr<Expression>> args_;
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

// Call:
// Represents a function call, or call to any other callable object.
//
// Examples:
//  * `f(a, b, c = 3)`
//  * `arg'func`
struct Call : public Expression {
  explicit Call(TextSpan span, std::unique_ptr<Expression> callee,
                core::OrderedFnArgs<Expression> args)
      : Expression(std::move(span)),
        callee_(std::move(callee)),
        args_(std::move(args)) {}

  ~Call() override {}

  Expression const *callee() const { return callee_.get(); }
  Expression *callee() { return callee_.get(); }
  core::FnArgs<Expression const *, std::string_view> const &args() const {
    return args_.args();
  }

  template <typename Fn>
  void Apply(Fn &&fn) {
    args_.Apply(std::forward<Fn>(fn));
  }

  std::pair<std::unique_ptr<Expression>, core::OrderedFnArgs<Expression>>
  extract() && {
    return std::pair{std::move(callee_), std::move(args_)};
  }

#include "visitor/visitors.xmacro.h"

 private:
  std::unique_ptr<Expression> callee_;
  core::OrderedFnArgs<Expression> args_;
};

// Cast:
// Represents a type-conversion. These can be either builtin conversion (for
// example, between integral types) or user-defined conversion via overloading
// the `as` operator. In either case, syntactically, they are represented by
// `<expr> as <type-expr>`.
//
// Examples:
//  * `3 as nat32`
//  * `null as *int64`
struct Cast : public Expression {
  explicit Cast(TextSpan span, std::unique_ptr<Expression> expr,
       std::unique_ptr<Expression> type_expr)
      : Expression(std::move(span)),
        expr_(std::move(expr)),
        type_(std::move(type_expr)) {}
  ~Cast() override {}

  Expression const *expr() const { return expr_.get(); }
  Expression *expr() { return expr_.get(); }
  Expression const *type() const { return type_.get(); }
  Expression *type() { return type_.get(); }

#include "visitor/visitors.xmacro.h"

 private:
  std::unique_ptr<Expression> expr_, type_;
};

// ChainOp:
// Represents a sequence of operators all of the same precedence. In may other
// languages these would be characterized as compositions of binary operators.
// Allowing chaining gives us more flexibility and enables us to treat some
// user-defined operators as variadic. The canonical example in this space is
// `+` for string concatenation. Today, `+` is treated as a binary operator, but
// we intend to move all of the Binop nodes into ChainOp.
//
// Example:
//  `a < b == c < d`
struct ChainOp : public Expression {
  // TODO consider having a construct-or-append static function.
  explicit ChainOp(TextSpan span, std::unique_ptr<Expression> expr)
      : Expression(std::move(span)) {
    exprs_.push_back(std::move(expr));
  }
  ~ChainOp() override {}

  void append(frontend::Operator op, std::unique_ptr<Expression> expr) {
    ops_.push_back(op);
    exprs_.push_back(std::move(expr));
  }

  NodeSpan<Expression const> exprs() const { return exprs_; }
  NodeSpan<Expression> exprs() { return exprs_; }
  absl::Span<frontend::Operator const> ops() const { return ops_; }
  absl::Span<frontend::Operator> ops() { return absl::MakeSpan(ops_); }

  std::vector<std::unique_ptr<Expression>> &&extract() && {
    return std::move(exprs_);
  }

#include "visitor/visitors.xmacro.h"

 private:
  std::vector<frontend::Operator> ops_;
  std::vector<std::unique_ptr<Expression>> exprs_;
};

// TODO
struct CommaList : public Expression {
  CommaList() = default;
  ~CommaList() override {}

  CommaList(CommaList const &) noexcept = default;
  CommaList(CommaList &&) noexcept      = default;
  CommaList &operator=(CommaList const &) noexcept = default;
  CommaList &operator=(CommaList &&) noexcept = default;

#include "visitor/visitors.xmacro.h"

  std::vector<std::unique_ptr<Expression>> &&extract() && {
    return std::move(exprs_);
  }
  bool needs_expansion() const override { return !parenthesized_; }

  std::vector<std::unique_ptr<Expression>> exprs_;
};

// EnumLiteral:
//
// Represents the literal expression evaluating to an enum-type or flags-type.
// The body consists of a collection of declarations of the enumerators. Each of
// which may be assigned a specific value.
//
// Example:
//  ```
//  Suit ::= enum {
//    CLUB
//    DIAMOND
//    HEART
//    SPADE
//  }
//  ```
//
//  `Color ::= flags { RED \\ BLUE \\ GREEN }`
//
//  ```
//  errno_values ::= enum {
//    SUCCESS ::=  0
//    EPERM   ::=  1  // Operation not permitted
//    ENOENT  ::=  2  // No such file or directory
//    ESRCH   ::=  3  // No such process
//    EINTR   ::=  4  // Interrupted system call
//    EIO     ::=  5  // I/O error
//    // ...
//  }
//  ```
//
struct EnumLiteral : ScopeExpr<core::DeclScope> {
  enum Kind : char { Enum, Flags };

  EnumLiteral(TextSpan span, std::vector<std::unique_ptr<Expression>> elems,
              Kind kind)
      : ScopeExpr<core::DeclScope>(std::move(span)),
        elems_(std::move(elems)),
        kind_(kind) {}

  ~EnumLiteral() override {}

  NodeSpan<Expression> elems() { return elems_; }
  NodeSpan<Expression const> elems() const { return elems_; }
  Kind kind() const { return kind_; }

#include "visitor/visitors.xmacro.h"

 private:
  std::vector<std::unique_ptr<Expression>> elems_;
  Kind kind_;
};

// TODO
struct FunctionLiteral : public Expression {
  // Represents a function with all constants bound to some value.
  FunctionLiteral(TextSpan span, Module *mod,
                  std::vector<std::unique_ptr<Declaration>> in_params,
                  std::vector<std::unique_ptr<Node>> statements,
                  std::optional<std::vector<std::unique_ptr<Expression>>>
                      out_params = std::nullopt)
      : Expression(std::move(span)),
        outputs_(std::move(out_params)),
        statements_(std::move(statements)),
        module_(mod) {
    for (auto &input : in_params) {
      input->flags() |= Declaration::f_IsFnParam;
      // NOTE: This is safe because the declaration is behind a unique_ptr so
      // the string is never moved. You need to be careful if you ever decide to
      // use make this declaration inline because SSO might mean moving the
      // declaration (which can happen if core::FnParams internal vector gets
      // reallocated) could invalidate the string_view unintentionally.
      std::string_view name = input->id();

      // Note the weird naming here: A declaration which is default initialized
      // means there is no `=` as part of the declaration. This means that the
      // declaration, when thougth of as a parameter to a function, has no
      // default value.
      core::FnParamFlags flags{};
      if (!input->IsDefaultInitialized()) { flags = core::HAS_DEFAULT; }

      inputs_.append(name, std::move(input), flags);
    }
  }
  FunctionLiteral(FunctionLiteral &&) noexcept = default;
  ~FunctionLiteral() override {}

#include "visitor/visitors.xmacro.h"

  std::unique_ptr<core::FnScope> fn_scope_;

  // Note this field is computed, but it is independent of any type or
  // context-specific information. It holds a topologically sorted list of
  // function parameters such that earlier parameters never depend on later
  // ones. It's filled in assign_scope because that's when we have enough
  // information to do so and it guarantees it's only called once.
  //
  // TODO rename assign_scope.
  std::vector<Declaration const *> sorted_params_;
  absl::flat_hash_map<Declaration const *, size_t> decl_to_param_;
  base::Graph<Declaration const *> param_dep_graph_;

  // TODO This is storing both the name in the declaration and pulls the
  // string_view of the name out in core::FnParams::Param.
  core::FnParams<std::unique_ptr<Declaration>> inputs_;
  std::optional<std::vector<std::unique_ptr<Expression>>> outputs_;
  std::vector<std::unique_ptr<Node>> statements_;

  Module *module_            = nullptr;
};

// Terminal:
// Represents any node that is not an identifier but has no sub-parts. These are
// typically numeric literals, or expressions that are also keywords such as
// `true`, `false`, or `null`.
struct Terminal : public Expression {
  explicit Terminal(TextSpan span, ir::Results results, type::Type const *t)
      : Expression(std::move(span)), results_(std::move(results)), type_(t) {}
  ~Terminal() override {}

  type::Type const *type() const { return type_; }
  ir::Results const &value() const { return results_; }
  template <typename T>
  T as() const {
    return results_.get<T>(0).val_;
  }

#include "visitor/visitors.xmacro.h"

 private:
  ir::Results results_;
  type::Type const *type_;
};

// Identifier:
// Represents any user-defined identifier.
struct Identifier : public Expression {
  Identifier(TextSpan span, std::string token)
      : Expression(std::move(span)), token_(std::move(token)) {}
  ~Identifier() override {}

#include "visitor/visitors.xmacro.h"

  std::string_view token() const { return token_; }
  Declaration const *decl() const { return decl_; }
  void set_decl(Declaration const *decl) { decl_ = decl; }

 private:
  std::string token_;
  Declaration const *decl_ = nullptr;
};

// Import:
// Represents a request from one module to use parts of a different module.
//
// Examples:
//  * `import "a_module.ic"`
//  * `import function_returning_a_string()`
struct Import : public Expression {
  explicit Import(TextSpan span, std::unique_ptr<Expression> expr)
      : Expression(std::move(span)), operand_(std::move(expr)) {}
  ~Import() override {}

  Expression const *operand() const { return operand_.get(); }
  Expression *operand() { return operand_.get(); }

#include "visitor/visitors.xmacro.h"

 private:
  std::unique_ptr<Expression> operand_;
};

// Index:
// Represents indexing into an array, buffer-pointer, or a call to the ([])
// operator if/when that may be overloaded.
//
// Example:
//  * `buf_ptr[3]`
//  * `my_array[4]`
struct Index : public Expression {
  explicit Index(TextSpan span, std::unique_ptr<Expression> lhs,
                 std::unique_ptr<Expression> rhs)
      : Expression(std::move(span)),
        lhs_(std::move(lhs)),
        rhs_(std::move(rhs)) {}
  ~Index() override {}

  Expression const* lhs() const { return lhs_.get(); }
  Expression* lhs() { return lhs_.get(); }
  Expression const* rhs() const { return rhs_.get(); }
  Expression* rhs() { return rhs_.get(); }
  std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>> extract()
      && {
    return std::pair(std::move(lhs_), std::move(rhs_));
  }

#include "visitor/visitors.xmacro.h"

 private:
  std::unique_ptr<Expression> lhs_, rhs_;
};

// Interface:
// Represents a literal defining an interface that a type may adhere to. Types
// which satisfy all the constraints specified in an interface may be passed
// into any function accepting an object of that interface.
//
// Example:
//  ```
//  comparable ::= interface {
//    (<)  :: (self, self) -> bool
//    (==) :: (self, self) -> bool
//  }
//  ```
//
// TODO this is a work in progress. The semantics of the declarations in this
// node have not been decided upon yet.
struct Interface : public ScopeExpr<core::DeclScope> {
  explicit Interface(TextSpan span,
                     std::vector<std::unique_ptr<Declaration>> decls)
      : ScopeExpr<core::DeclScope>(std::move(span)), decls_(std::move(decls)) {}
  ~Interface() override {}

  NodeSpan<Declaration const> decls() const { return decls_; }
  NodeSpan<Declaration> decls() { return decls_; }

#include "visitor/visitors.xmacro.h"

 private:
  std::vector<std::unique_ptr<Declaration>> decls_;
};

// Jump:
// Represents a statement describing where a block should jump after completion.
//
// Example (in context of a scope):
//  ```
//  while ::= scope {
//    init ::= (b: bool) -> () {
//      switch (b) {
//        jump do()   when true              // <-- `jump do()`
//        jump exit() when false             // <-- `jump exit()`
//      }
//    }
//    do ::= block {
//      before ::= () -> () {}
//      after ::= () -> () { jump start() }  // <-- `jump start()`
//    }
//    done ::= () -> () {}
//  }
//  ```
struct Jump : public Node {
 public:
  explicit Jump(TextSpan span, std::vector<std::unique_ptr<Call>> calls)
      : Node(std::move(span)) {
    for (auto &call : calls) {
      auto[callee, ordered_args] = std::move(*call).extract();
      if (auto *term = callee->if_as<Terminal>()) {
        if (term->as<ir::BlockDef *>() == ir::BlockDef::Start()) {
          options_.emplace_back("start", std::move(ordered_args).DropOrder());
        } else if (term->as<ir::BlockDef *>() == ir::BlockDef::Exit()) {
          options_.emplace_back("exit", std::move(ordered_args).DropOrder());
        } else {
          UNREACHABLE();
        }
      } else if (auto *id = callee->if_as<Identifier>()) {
        options_.emplace_back(std::string{id->token()},
                              std::move(ordered_args).DropOrder());
      } else {
        UNREACHABLE();
      }
    }
  }

#include "visitor/visitors.xmacro.h"

  // TODO private:

  // A jump option is a collection of blocks that may be jumped to and the
  // arguments to pass to such a block. When evaluating jump options, the option
  // is chose if the collection of blocks refers to a block that is present on
  // the scope node. In that case, the arguments are evaluated and passed to it.
  // Otherwise, the option is dicarded and the next option in the `options_`
  // container is chosen.
  struct JumpOption {
    explicit JumpOption(std::string name,
                        core::FnArgs<std::unique_ptr<Expression>> a)
        : block(std::move(name)), args(std::move(a)) {}
    JumpOption(JumpOption const &)     = default;
    JumpOption(JumpOption &&) noexcept = default;
    JumpOption &operator=(JumpOption const &) = default;
    JumpOption &operator=(JumpOption &&) noexcept = default;

    std::string block;
    core::FnArgs<std::unique_ptr<Expression>> args;
  };
  // A jump will evaluate at compile-time to the first option for which the
  // scope node has all possible blocks.
  std::vector<JumpOption> options_;
};

// JumpHandler:
// TODO figure out a good name for this node and the precise syntax/semantics it
// is going to have.
//
// Example:
//  ```
//  jump_handler (b: bool) {
//    if (b) then { jump next() }
//    jump exit()
//  }
//  ```
struct JumpHandler : ScopeExpr<core::FnScope> {
  explicit JumpHandler(TextSpan span,
                       std::vector<std::unique_ptr<Declaration>> input,
                       std::vector<std::unique_ptr<Node>> stmts)
      : ScopeExpr<core::FnScope>(std::move(span)),
        input_(std::move(input)),
        stmts_(std::move(stmts)) {
    for (auto &input : input_) { input->flags() |= Declaration::f_IsFnParam; }
  }

#include "visitor/visitors.xmacro.h"
  NodeSpan<Declaration const> input() const { return input_; }
  NodeSpan<Declaration> input() { return input_; }
  NodeSpan<Node> stmts() { return stmts_; }
  NodeSpan<Node const> stmts() const { return stmts_; }

 private:
  std::vector<std::unique_ptr<Declaration>> input_;
  std::vector<std::unique_ptr<Node>> stmts_;
};

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

// ScopeLiteral:
// Represents the definition of a user-defined scope, including how blocks jump
// to one another.
//
// Example:
//  ```
//  if ::= scope {
//    init ::= (b: bool) -> () {
//      switch (b) {
//        jump then()           when true
//        jump (else | exit)()  when false
//      }
//    }
//    then ::= block {
//      before ::= () -> () {}
//      after ::= () -> () { jump exit() }
//    }
//    else ::= block? {
//      before ::= () -> () {}
//      after ::= () -> () { jump exit() }
//    }
//    done ::= () -> () {}
//  }
//  ```
struct ScopeLiteral : public ScopeExpr<core::ScopeLitScope> {
  ScopeLiteral(TextSpan span, std::vector<std::unique_ptr<Declaration>> decls)
      : ScopeExpr<core::ScopeLitScope>(std::move(span)),
        decls_(std::move(decls)) {}
  ~ScopeLiteral() override {}

  NodeSpan<Declaration const> decls() const { return decls_; }
  NodeSpan<Declaration> decls() { return decls_; }

#include "visitor/visitors.xmacro.h"

 private:
  std::vector<std::unique_ptr<Declaration>> decls_;
};

// ScopeNode:
// 
// Represents the usage of a scope such as `if`, `while`, or any other
// user-defined scope. This encompasses all blocks (e.g., in the case of `if`,
// it encompasses the `then` and `else` blocks if they are present.
//
// Examples:
//  ```
//  if (condition1) then {
//    do_something()
//  } else if (condition2) then {
//    do_something_else()
//  } else {
//    do_third_thing()
//  }
//  ```
//
//  `unwrap (maybe_object) or { return -1 }`
//
struct ScopeNode : public Expression {
  ScopeNode(TextSpan span, std::unique_ptr<Expression> name,
            core::OrderedFnArgs<Expression> args, std::vector<BlockNode> blocks)
      : Expression(std::move(span)),
        name_(std::move(name)),
        args_(std::move(args)),
        blocks_(std::move(blocks)) {}

  ~ScopeNode() override {}

  Expression const *name() const { return name_.get(); }
  Expression *name() { return name_.get(); }
  core::FnArgs<Expression const *, std::string_view> const &args() const {
    return args_.args();
  }

  template <typename Fn>
  void Apply(Fn &&fn) {
    args_.Apply(std::forward<Fn>(fn));
  }

  absl::Span<BlockNode const> blocks() const { return blocks_; }
  absl::Span<BlockNode> blocks() { return absl::MakeSpan(blocks_); }

  // Appends the given block not necessarily to this ScopeNode, but to the scope
  // that makes sense syntactically. For instance, in the first example above,
  // the inner `if` ScopeNode checking `condition2` would be appended to.
  void append_block_syntactically(BlockNode block, ScopeNode *updated_last_scope_node = nullptr) {
    auto * scope_node = (last_scope_node_ ? last_scope_node_ : this);
    scope_node->blocks_.push_back(std::move(block));
    if (updated_last_scope_node) { last_scope_node_ = updated_last_scope_node; }
  }

#include "visitor/visitors.xmacro.h"

 private:
  std::unique_ptr<Expression> name_;
  core::OrderedFnArgs<Expression> args_;
  std::vector<BlockNode> blocks_;
  ScopeNode *last_scope_node_ = nullptr;
};

// TODO
struct StructLiteral : public Expression {
  StructLiteral()                          = default;
  StructLiteral(StructLiteral &&) noexcept = default;
  ~StructLiteral() override {}

  StructLiteral &operator=(StructLiteral &&) noexcept = default;

#include "visitor/visitors.xmacro.h"

  std::unique_ptr<core::DeclScope> type_scope;
  std::vector<Declaration> fields_, args_;
};

// TODO
struct StructType : public Expression {
  StructType(TextSpan span) : Expression(span) {}
  ~StructType() override {}

#include "visitor/visitors.xmacro.h"

  std::vector<std::unique_ptr<Expression>> args_;
};

// TODO comment
// TODO consider separating this into two classes given that we know when we
// parse if it has parens or not.
struct Switch : public Expression {
  ~Switch() override {}

#include "visitor/visitors.xmacro.h"

  std::unique_ptr<Expression> expr_;
  std::vector<std::pair<std::unique_ptr<Node>, std::unique_ptr<Expression>>>
      cases_;
};

// TODO
struct Unop : public Expression {
  ~Unop() override {}

#include "visitor/visitors.xmacro.h"

  bool needs_expansion() const override {
    return !parenthesized_ && op == frontend::Operator::Expand;
  }

  std::unique_ptr<Expression> operand;
  frontend::Operator op;
};

}  // namespace ast

#endif  // ICARUS_AST_AST_H
