#ifndef ICARUS_AST_AST_H
#define ICARUS_AST_AST_H

#include <memory>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

#include "absl/types/span.h"
#include "ast/build_param_dependency_graph.h"
#include "ast/expression.h"
#include "ast/jump_options.h"
#include "ast/node.h"
#include "ast/scope/decl.h"
#include "ast/scope/fn.h"
#include "ast/scope/module.h"
#include "ast/scope/scope.h"
#include "ast/scope/scope_lit.h"
#include "ast/visitor_base.h"
#include "base/ptr_span.h"
#include "core/arguments.h"
#include "core/ordered_arguments.h"
#include "core/params.h"
#include "frontend/lex/operators.h"
#include "ir/value/addr.h"
#include "ir/value/builtin_fn.h"
#include "ir/value/label.h"
#include "ir/value/value.h"

namespace ast {

void InitializeNodes(base::PtrSpan<Node> nodes, Scope *scope);

#define ICARUS_AST_VIRTUAL_METHODS                                             \
  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple)                \
      const override {                                                         \
    visitor->ErasedVisit(this, ret, arg_tuple);                                \
  }                                                                            \
                                                                               \
  void DebugStrAppend(std::string *out, size_t indent) const override;         \
  void Initialize(Scope *scope) override;                                      \
  bool IsDependent() const override

// WithScope:
// A mixin which adds a scope of the given type `S`.
template <typename S>
struct WithScope {
  template <typename... Args>
  void set_body_with_parent(Scope *p, Args &&... args) {
    body_scope_ = p->template add_child<S>(std::forward<Args>(args)...);
  }
  S *body_scope() const { return body_scope_.get(); }

 private:
  std::unique_ptr<S> body_scope_ = nullptr;
};

// Access:
// Represents member access with the `.` operator.
//
// Examples:
//  * `my_pair.first_element`
//  * `(some + computation).member`
struct Access : Expression {
  explicit Access(frontend::SourceRange const &range,
                  std::unique_ptr<Expression> operand, std::string member_name)
      : Expression(range),
        operand_(std::move(operand)),
        member_name_(std::move(member_name)) {}
  constexpr std::string_view member_name() const { return member_name_; }
  constexpr frontend::SourceRange member_range() const {
    return frontend::SourceRange(operand_->range().end(), range().end());
  }
  Expression const *operand() const { return operand_.get(); }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::unique_ptr<Expression> operand_;
  std::string member_name_;
};

// ArgumentType:
// Reperesnts the type of the argument bound to a parameter of the given name in
// generic parameter lists. Syntactically, this is spelled with a `$` optionally
// followed by the name of a parameter. If missing, the name is implicitly the
// name of the surrounding declaration. For example,
//
// ```
// f ::= (x: $, y: $x) -> () { ... }
// ```
//
// In this example, `f` is a generic function that accepts two arguments of the
// same type. The types of the parameter `x` will be inferred from the argument
// bound to `x`. The type of the parameter `y` must match the type of the
// argument bound to `x`.
struct ArgumentType : Expression {
  explicit ArgumentType(frontend::SourceRange const &range, std::string name)
      : Expression(range), name_(std::move(name)) {}
  std::string_view name() const { return name_; }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::string name_;
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
struct ArrayLiteral : Expression {
  explicit ArrayLiteral(frontend::SourceRange const &range,
                        std::unique_ptr<Expression> elem)
      : Expression(range) {
    elems_.push_back(std::move(elem));
  }
  ArrayLiteral(frontend::SourceRange const &range,
               std::vector<std::unique_ptr<Expression>> elems)
      : Expression(range), elems_(std::move(elems)) {}
  bool empty() const { return elems_.empty(); }
  size_t size() const { return elems_.size(); }
  Expression const *elem(size_t i) const { return elems_[i].get(); }
  base::PtrSpan<Expression const> elems() const { return elems_; }
  auto extract() && { return std::move(elems_); }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::vector<std::unique_ptr<Expression>> elems_;
};

// Assignment:
// Represents an assignment of one or more values to one or more references.
// The number of values must match the number of references.
//
// Examples:
// * `a = b`
// * `(a, b) = (c, d)`
//
// TODO: Because assignments can appear as nodes in call expressions temporarily
// during parsing, we treat assignments as expressions. This debt should be paid
// down.
struct Assignment : Expression {
  explicit Assignment(frontend::SourceRange const &range,
                      std::vector<std::unique_ptr<Expression>> lhs,
                      std::vector<std::unique_ptr<Expression>> rhs)
      : Expression(range), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}
  base::PtrSpan<Expression const> lhs() const { return lhs_; }
  base::PtrSpan<Expression const> rhs() const { return rhs_; }

  auto extract() && { return std::pair(std::move(lhs_), std::move(rhs_)); }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::vector<std::unique_ptr<Expression>> lhs_;
  std::vector<std::unique_ptr<Expression>> rhs_;
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
struct ArrayType : Expression {
  explicit ArrayType(frontend::SourceRange const &range,
                     std::unique_ptr<Expression> length,
                     std::unique_ptr<Expression> data_type)
      : Expression(range), data_type_(std::move(data_type)) {
    lengths_.push_back(std::move(length));
  }
  ArrayType(frontend::SourceRange const &range,
            std::vector<std::unique_ptr<Expression>> lengths,
            std::unique_ptr<Expression> data_type)
      : Expression(range),
        lengths_(std::move(lengths)),
        data_type_(std::move(data_type)) {}
  base::PtrSpan<Expression const> lengths() const { return lengths_; }
  Expression const *length(size_t i) const { return lengths_[i].get(); }
  Expression const *data_type() const { return data_type_.get(); }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::vector<std::unique_ptr<Expression>> lengths_;
  std::unique_ptr<Expression> data_type_;
};

// BinaryOperator:
// Represents a call to a binary operator.
//
// Examples:
//  * `thing1 + thing2`
//  * `3 * (x + y)`
//
// Note that some things one might expect to be binary operators are treated
// differently (see `ComparisonOperator`). This is because in Icarus, operators
// such as `==` allow chains so that `x == y == z` can evaluate to `true` if and
// only if both `x == y` and `y == z`.
struct BinaryOperator : Expression {
  explicit BinaryOperator(std::unique_ptr<Expression> lhs,
                          frontend::Operator op,
                          std::unique_ptr<Expression> rhs)
      : Expression(
            frontend::SourceRange(lhs->range().begin(), rhs->range().end())),
        op_(op),
        lhs_(std::move(lhs)),
        rhs_(std::move(rhs)) {}
  Expression const *lhs() const { return lhs_.get(); }
  Expression const *rhs() const { return rhs_.get(); }
  frontend::Operator op() const { return op_; }

  auto extract() && { return std::pair{std::move(lhs_), std::move(rhs_)}; }

  ICARUS_AST_VIRTUAL_METHODS;

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
struct Declaration : Expression {
  using Flags                         = uint8_t;
  static constexpr Flags f_IsFnParam  = 0x01;
  static constexpr Flags f_IsOutput   = 0x02;
  static constexpr Flags f_IsConst    = 0x04;
  static constexpr Flags f_InitIsHole = 0x08;

  explicit Declaration(frontend::SourceRange const &range, std::string id,
                       frontend::SourceRange const &id_range,
                       std::unique_ptr<Expression> type_expression,
                       std::unique_ptr<Expression> initial_val, Flags flags)
      : Expression(range),
        id_(std::move(id)),
        id_range_(id_range),
        type_expr_(std::move(type_expression)),
        init_val_(std::move(initial_val)),
        flags_(flags) {}
  Declaration(Declaration &&) noexcept = default;
  Declaration &operator=(Declaration &&) noexcept = default;

  // TODO: These functions are confusingly named. They look correct in normal
  // declarations, but in function arguments, IsDefaultInitialized() is true iff
  // there is no default value provided.
  bool IsInferred() const { return not type_expr_; }
  bool IsDefaultInitialized() const {
    return not init_val_ and not IsUninitialized();
  }
  bool IsCustomInitialized() const { return init_val_.get(); }
  bool IsUninitialized() const { return (flags_ & f_InitIsHole); }

  enum Kind {
    kDefaultInit              = 0,
    kCustomInit               = 2,
    kInferred                 = 3,
    kUninitialized            = 6,
    kInferredAndUninitialized = 7,  // This is an error
  };
  Kind kind() const {
    int k = IsInferred() ? 1 : 0;
    if (IsUninitialized()) { k |= kUninitialized; }
    if (IsCustomInitialized()) { k |= kCustomInit; }
    return static_cast<Kind>(k);
  }

  std::string_view id() const { return id_; }
  frontend::SourceRange const &id_range() const { return id_range_; }
  Expression const *type_expr() const { return type_expr_.get(); }
  Expression const *init_val() const { return init_val_.get(); }

  std::tuple<std::string, std::unique_ptr<Expression>,
             std::unique_ptr<Expression>>
  extract() && {
    return std::make_tuple(std::move(id_), std::move(type_expr_),
                           std::move(init_val_));
  }

  module::BasicModule const *module() const {
    return scope_->Containing<ModuleScope>()->module();
  }

  Flags flags() const { return flags_; }
  Flags &flags() { return flags_; }  // TODO consider removing this.

  void set_initial_value(std::unique_ptr<Expression> expr);

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::string id_;
  frontend::SourceRange id_range_;
  std::unique_ptr<Expression> type_expr_, init_val_;
  Flags flags_;
};

// ParameterizedExpression:
// This is a parent-class for all nodes that have parameters, allowing us to
// handle those parameters uniformly. oreover, this gives us the ability to key
// hash-tables on `ParameterizedExpression const *`.
struct ParameterizedExpression : Expression {
  explicit ParameterizedExpression(frontend::SourceRange const &range)
      : Expression(range) {}

  explicit ParameterizedExpression(
      frontend::SourceRange const &range,
      std::vector<std::unique_ptr<Declaration>> params)
      : Expression(range) {
    for (auto &param : params) {
      // NOTE: It's save to save a `std::string_view` to a parameter because
      // both the declaration and this will live for the length of the syntax
      // tree.
      params_.append(param->id(), std::move(param));
    }

    InitializeParams();
  }

  // TODO params() should be a reference to core::Params?
  using params_type = core::Params<std::unique_ptr<Declaration>>;
  params_type const &params() const { return params_; }

  // Returns a sequence of (parameter-index, dependency-node) pairs ordered in
  // such a way that each has no dependencies on any that come after it.
  absl::Span<std::pair<int, core::DependencyNode<ast::Declaration>> const>
  ordered_dependency_nodes() const {
    return ordered_dependency_nodes_;
  }

  // Returns true if the expression accepts a generic parameter (i.e., a
  // constant parameter or a parameter with a deduced type).
  constexpr bool is_generic() const { return is_generic_; }

 protected:
  void InitializeParams() {
    for (auto &param : params_) {
      param.value->flags() |= Declaration::f_IsFnParam;
      if (not param.value->IsDefaultInitialized()) {
        param.flags = core::HAS_DEFAULT;
      }
      if (not is_generic_) {
        is_generic_ = (param.value->flags() & Declaration::f_IsConst) or
                      param.value->IsDependent();
      }
    }
  }

  core::Params<std::unique_ptr<ast::Declaration>> params_;
  std::vector<std::pair<int, core::DependencyNode<ast::Declaration>>>
      ordered_dependency_nodes_;
  bool is_generic_ = false;
};

// DesignatedInitializer:
//
// Represents an initializer for the given struct.
//
// Example:
// For the struct `S` with fields `x` and `y`, we might write
//
// ```
// S.{
//   x = 3
//   y = 17
// }
// ```
//
// Note that it is an invariant of this struct that the left-hand side of every
// assigment returned in `assignments()` is an `Identifier`.
//
// TODO: Consider using a stronger type than `Assignment` that guarantees
// left-hand sides are identifiers.
struct DesignatedInitializer : Expression {
  // The `assignments` passed in to this constructor must have all left-hand
  // sides be `Identifier`s.
  DesignatedInitializer(frontend::SourceRange const &range,
                        std::unique_ptr<Expression> type,
                        std::vector<std::unique_ptr<Assignment>> assignments)
      : Expression(range),
        type_(std::move(type)),
        assignments_(std::move(assignments)) {
    for (auto const *assignment : this->assignments()) {
      for (auto const *expr : assignment->lhs()) {
        ASSERT(expr->is<ast::Identifier>() == true);
      }
    }
  }

  Expression const *type() const { return type_.get(); }
  base::PtrSpan<Assignment const> assignments() const { return assignments_; }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::unique_ptr<Expression> type_;
  std::vector<std::unique_ptr<Assignment>> assignments_;
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
struct BlockLiteral : Expression, WithScope<DeclScope> {
  explicit BlockLiteral(frontend::SourceRange const &range,
                        std::vector<std::unique_ptr<Declaration>> before,
                        std::vector<std::unique_ptr<Declaration>> after)
      : Expression(range),
        before_(std::move(before)),
        after_(std::move(after)) {}
  base::PtrSpan<Declaration const> before() const { return before_; }
  base::PtrSpan<Declaration const> after() const { return after_; }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::vector<std::unique_ptr<Declaration>> before_, after_;
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
// (likely in the form of `core::Arguments<std::unique_ptr<ast::Expression>>`).
struct BlockNode : ParameterizedExpression, WithScope<ExecScope> {
  explicit BlockNode(frontend::SourceRange const &range, std::string name,
                     std::vector<std::unique_ptr<Node>> stmts)
      : ParameterizedExpression(range),
        name_(std::move(name)),
        stmts_(std::move(stmts)) {}
  explicit BlockNode(frontend::SourceRange const &range, std::string name,
                     std::vector<std::unique_ptr<Declaration>> params,
                     std::vector<std::unique_ptr<Node>> stmts)
      : ParameterizedExpression(range, std::move(params)),
        name_(std::move(name)),
        stmts_(std::move(stmts)) {}
  BlockNode(BlockNode &&) noexcept = default;
  BlockNode &operator=(BlockNode &&) noexcept = default;

  std::string_view name() const { return name_; }
  base::PtrSpan<Node const> stmts() const { return stmts_; }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::string name_;
  std::vector<std::unique_ptr<Node>> stmts_;
};

// Represents a builtin (possibly generic) function. Examples include `foreign`,
// which declares a foreign-function by name, or `opaque` which constructs a new
// type with no known size or alignment (users can pass around pointers to
// values of an opaque type, but not actual values).
struct BuiltinFn : Expression {
  explicit BuiltinFn(frontend::SourceRange const &range, ir::BuiltinFn b)
      : Expression(range), val_(b) {}
  ir::BuiltinFn value() const { return val_; }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  ir::BuiltinFn val_;
};

// Call:
// Represents a function call, or call to any other callable object.
//
// Examples:
//  * `f(a, b, c = 3)`
//  * `arg'func`
struct Call : Expression {
  explicit Call(frontend::SourceRange const &range,
                std::unique_ptr<Expression> callee,
                core::OrderedArguments<Expression> args)
      : Expression(range), callee_(std::move(callee)), args_(std::move(args)) {}
  Expression const *callee() const { return callee_.get(); }
  core::Arguments<Expression const *> const &args() const {
    return args_.args();
  }

  auto extract() && { return std::pair{std::move(callee_), std::move(args_)}; }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::unique_ptr<Expression> callee_;
  core::OrderedArguments<Expression> args_;
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
struct Cast : Expression {
  explicit Cast(frontend::SourceRange const &range,
                std::unique_ptr<Expression> expr,
                std::unique_ptr<Expression> type_expr)
      : Expression(range),
        expr_(std::move(expr)),
        type_(std::move(type_expr)) {}
  Expression const *expr() const { return expr_.get(); }
  Expression const *type() const { return type_.get(); }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::unique_ptr<Expression> expr_, type_;
};

// ComparisonOperator:
// Represents a sequence of operators all of the same precedence. In may other
// languages these would be characterized as compositions of binary operators.
// Allowing chaining gives us more flexibility and enables us to treat some
// user-defined operators as variadic. The canonical example in this space is
// `+` for string concatenation. Today, `+` is treated as a binary operator, but
// we intend to move all of the BinaryOperator nodes into ComparisonOperator.
//
// Example:
//  `a < b == c < d`
struct ComparisonOperator : Expression {
  // TODO consider having a construct-or-append static function.
  explicit ComparisonOperator(frontend::SourceRange const &range,
                              std::unique_ptr<Expression> expr)
      : Expression(range) {
    exprs_.push_back(std::move(expr));
  }

  void append(frontend::Operator op, std::unique_ptr<Expression> expr) {
    ops_.push_back(op);
    exprs_.push_back(std::move(expr));
  }

  base::PtrSpan<Expression const> exprs() const { return exprs_; }
  absl::Span<frontend::Operator const> ops() const { return ops_; }

  // Returns a source range consisting of the `i`th (zero-indexed) operator and
  // surrounding expressions in this chain of comparison operators.
  frontend::SourceRange binary_range(size_t i) const {
    return frontend::SourceRange(exprs_[i]->range().begin(),
                                 exprs_[i + 1]->range().end());
  }

  auto extract() && { return std::move(exprs_); }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::vector<frontend::Operator> ops_;
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
struct EnumLiteral : Expression, WithScope<DeclScope> {
  enum Kind : char { Enum, Flags };

  EnumLiteral(
      frontend::SourceRange const &range, std::vector<std::string> enumerators,
      absl::flat_hash_map<std::string, std::unique_ptr<Expression>> values,
      Kind kind)
      : Expression(range),
        enumerators_(std::move(enumerators)),
        values_(std::move(values)),
        kind_(kind) {}
  absl::Span<std::string const> enumerators() const { return enumerators_; }
  absl::flat_hash_map<std::string, std::unique_ptr<Expression>> const &
  specified_values() const {
    return values_;
  }
  Kind kind() const { return kind_; }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::vector<std::string> enumerators_;
  absl::flat_hash_map<std::string, std::unique_ptr<Expression>> values_;
  Kind kind_;
};

// FunctionLiteral:
//
// Represents a literal function. Functions may have deduced return-type, which
// can be determined by calling `outputs()`, If the result is std::nullopt, the
// output is deduced. Functions may be generic.
//
// Examples:
// * `(n: int32) -> () { print n }`
// * `() -> int32 { return 3 }`
// * `(n: int32, m: int32) => n * m`
// * `(T :: type, val: T) => val`
//
struct FunctionLiteral : ParameterizedExpression, WithScope<FnScope> {
  explicit FunctionLiteral(
      frontend::SourceRange const &range,
      std::vector<std::unique_ptr<Declaration>> in_params,
      std::vector<std::unique_ptr<Node>> stmts,
      std::optional<std::vector<std::unique_ptr<Expression>>> out_params =
          std::nullopt)
      : ParameterizedExpression(range, std::move(in_params)),
        outputs_(std::move(out_params)),
        stmts_(std::move(stmts)) {}
  base::PtrSpan<Node const> stmts() const { return stmts_; }

  std::optional<base::PtrSpan<Expression const>> outputs() const {
    if (not outputs_) { return std::nullopt; }
    return *outputs_;
  }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::optional<std::vector<std::unique_ptr<Expression>>> outputs_;
  std::vector<std::unique_ptr<Node>> stmts_;
};

// FunctionType:
//
// Represents an expression involving the `->` token, which represnts a function
// type. Note that despite being embedded inside function literals,
// `FunctionLiteral` does not explicitly rely on `FunctionType`. This is
// because `FunctionType` may involve function types where the input
// parameters are not declarations.
//
// Examples:
// * `int64 -> bool`
// * `(b: bool) -> ()`
// * `(n: int64, b: bool) -> (float32, float32)
//
struct FunctionType : Expression {
  FunctionType(frontend::SourceRange const &range,
               std::vector<std::unique_ptr<Expression>> params,
               std::vector<std::unique_ptr<Expression>> output)
      : Expression(range),
        params_(std::move(params)),
        output_(std::move(output)) {}
  base::PtrSpan<Expression const> params() const { return params_; }
  base::PtrSpan<Expression const> outputs() const { return output_; }

  ICARUS_AST_VIRTUAL_METHODS;

  auto extract() && {
    return std::pair(std::move(params_), std::move(output_));
  }

 private:
  std::vector<std::unique_ptr<Expression>> params_;
  std::vector<std::unique_ptr<Expression>> output_;
};

// Identifier:
// Represents any user-defined identifier.
struct Identifier : Expression {
  Identifier(frontend::SourceRange const &range, std::string name)
      : Expression(range), name_(std::move(name)) {}

  ICARUS_AST_VIRTUAL_METHODS;

  std::string_view name() const { return name_; }

  std::string extract() && { return std::move(name_); }

 private:
  std::string name_;
};

// Import:
// Represents a request from one module to use parts of a different module.
//
// Examples:
//  * `import "a_module.ic"`
//  * `import function_returning_a_string()`
struct Import : Expression {
  explicit Import(frontend::SourceRange const &range,
                  std::unique_ptr<Expression> expr)
      : Expression(range), operand_(std::move(expr)) {}

  Expression const *operand() const { return operand_.get(); }

  ICARUS_AST_VIRTUAL_METHODS;

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
struct Index : Expression {
  explicit Index(frontend::SourceRange const &range,
                 std::unique_ptr<Expression> lhs,
                 std::unique_ptr<Expression> rhs)
      : Expression(range), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}

  Expression const *lhs() const { return lhs_.get(); }
  Expression const *rhs() const { return rhs_.get(); }
  auto extract() && { return std::pair(std::move(lhs_), std::move(rhs_)); }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::unique_ptr<Expression> lhs_, rhs_;
};

// Label:
// Represents a label which can be the target of a yield statement in a scope.
// Other languages used labels for "labelled-break", and this is a similar idea.
//
// Example:
// `#.my_label`
struct Label : Expression {
  explicit Label(frontend::SourceRange const &range, std::string label)
      : Expression(range), label_(std::move(label)) {}

  ir::Label value() const { return ir::Label(&label_); }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::string label_;
};

// Jump:
// Represents a component of a scope definition that directs control flow.
//
// Example:
// If the bool is true, control flow continues to the next block in this scope.
// Otherwise, control flow exits the scope entirely.
//  ```
//  jump (b: bool) {
//    if (b) then { goto next() }
//    goto exit()
//  }
//  ```
struct Jump : ParameterizedExpression, WithScope<FnScope> {
  explicit Jump(frontend::SourceRange const &range,
                std::unique_ptr<ast::Declaration> state,
                std::vector<std::unique_ptr<Declaration>> in_params,
                std::vector<std::unique_ptr<Node>> stmts)
      : ParameterizedExpression(range, std::move(in_params)),
        state_(std::move(state)),
        stmts_(std::move(stmts)) {
    if (state_) { state_->flags() |= Declaration::f_IsFnParam; }
  }

  ICARUS_AST_VIRTUAL_METHODS;

  Declaration const *state() const { return state_.get(); }
  base::PtrSpan<Node const> stmts() const { return stmts_; }

 private:
  std::unique_ptr<ast::Declaration> state_;
  std::vector<std::unique_ptr<Node>> stmts_;
};

// ParameterizedStructLiteral:
//
// Represents the definition of a parameterized user-defined structure. This
// consists of a collection of declarations, and a collection of parameters on
// which those declaration's types and initial values may depend.
//
// Examples:
// ```
// struct (T: type) {
//   ptr: *T
// }
//
// struct (N: int64) {
//   val: int64 = N
//   square ::= N * N
// }
// ```
struct ParameterizedStructLiteral : ParameterizedExpression,
                                    WithScope<DeclScope> {
  ParameterizedStructLiteral(frontend::SourceRange const &range,
                             std::vector<std::unique_ptr<Declaration>> params,
                             std::vector<Declaration> fields)
      : ParameterizedExpression(range, std::move(params)),
        fields_(std::move(fields)) {}

  absl::Span<Declaration const> fields() const { return fields_; }

  ParameterizedStructLiteral &operator        =(
      ParameterizedStructLiteral &&) noexcept = default;

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::vector<Declaration> fields_;
};

// ReturnStmt:
// Represents a return statement. Arbitrarily many expressions can be passed.
//
// Example:
//  ```
//  return "hello", 42
//  ```
//
struct ReturnStmt : Node {
  explicit ReturnStmt(frontend::SourceRange const &range,
                      std::vector<std::unique_ptr<Expression>> exprs = {})
      : Node(range), exprs_(std::move(exprs)) {}

  base::PtrSpan<Expression const> exprs() const { return exprs_; }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
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
//      goto b, then(), else() | exit()
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
struct ScopeLiteral : Expression, WithScope<ScopeLitScope> {
  explicit ScopeLiteral(frontend::SourceRange const &range,
                        std::unique_ptr<Expression> state_type,
                        std::vector<Declaration> decls)
      : Expression(range),
        state_type_(std::move(state_type)),
        decls_(std::move(decls)) {}

  absl::Span<Declaration const> decls() const { return decls_; }
  Expression const *state_type() const { return state_type_.get(); }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::unique_ptr<Expression> state_type_;
  std::vector<Declaration> decls_;
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
struct ScopeNode : Expression {
  ScopeNode(frontend::SourceRange const &range,
            std::unique_ptr<Expression> name,
            core::OrderedArguments<Expression> args,
            std::vector<BlockNode> blocks)
      : Expression(range),
        name_(std::move(name)),
        args_(std::move(args)),
        blocks_(std::move(blocks)) {}

  Expression const *name() const { return name_.get(); }
  core::Arguments<Expression const *> const &args() const {
    return args_.args();
  }

  template <typename Fn>
  void Apply(Fn &&fn) const {
    args_.Apply(std::forward<Fn>(fn));
  }

  absl::Span<BlockNode const> blocks() const { return blocks_; }

  ast::Label const *label() const { return label_ ? &*label_ : nullptr; }
  void set_label(Label label) { label_ = std::move(label); }

  // Appends the given block not necessarily to this ScopeNode, but to the scope
  // that makes sense syntactically. For instance, in the first example above,
  // the inner `if` ScopeNode checking `condition2` would be appended to.
  void append_block_syntactically(
      BlockNode block, ScopeNode *updated_last_scope_node = nullptr) {
    auto *scope_node = (last_scope_node_ ? last_scope_node_ : this);
    scope_node->blocks_.push_back(std::move(block));
    if (updated_last_scope_node) { last_scope_node_ = updated_last_scope_node; }
  }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::optional<Label> label_;
  std::unique_ptr<Expression> name_;
  core::OrderedArguments<Expression> args_;
  std::vector<BlockNode> blocks_;
  ScopeNode *last_scope_node_ = nullptr;
};

// ShortFunctionLiteral:
//
// Represents a literal function which is syntactically "short". That is, it
// uses `=>`, has it's return type inferred and has its body consist of a single
// expression.
//
// Examples:
// * `(n: int32, m: int32) => n * m`
// * `(T :: type, val: T) => val`
// * `(x: $x) => x`
//
struct ShortFunctionLiteral : ParameterizedExpression, WithScope<FnScope> {
  explicit ShortFunctionLiteral(
      frontend::SourceRange const &range,
      std::vector<std::unique_ptr<Declaration>> params,
      std::unique_ptr<Expression> body)
      : ParameterizedExpression(range, std::move(params)),
        body_(std::move(body)) {}
  Expression const *body() const { return body_.get(); }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::unique_ptr<Expression> body_;
};

// StructLiteral:
//
// Represents the definition of a user-defined structure. This consists of a
// collection of declarations.
//
// Examples:
// ```
// struct {
//   x: float64
//   y: float64
// }
//
// struct {}
// ```
struct StructLiteral : Expression, WithScope<DeclScope> {
  explicit StructLiteral(frontend::SourceRange const &range,
                         std::vector<Declaration> fields)
      : Expression(range), fields_(std::move(fields)) {}

  absl::Span<Declaration const> fields() const { return fields_; }

  StructLiteral &operator=(StructLiteral &&) noexcept = default;

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::vector<Declaration> fields_;
};

// Terminal:
// Represents any node that is not an identifier but has no sub-parts. These are
// typically numeric literals, or expressions that are also keywords such as
// `true`, `false`, or `null`.
struct Terminal : Expression {
  explicit Terminal(frontend::SourceRange const &range, ir::Value value)
      : Expression(range), value_(std::move(value)) {}
  ir::Value const &value() const { return value_; }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  ir::Value value_;
};

// UnaryOperator:
//
// Represents a call to a unary operator.
//
// Examples:
//  * `-some_number`
//  * `!some_boolean`
//  * `what_type_am_i:?`
//  * `@some_ptr`
struct UnaryOperator : Expression {
  enum class Kind {
    Copy,
    Init,
    Move,
    BufferPointer,
    TypeOf,
    Evaluate,
    At,
    Pointer,
    Address,
    Negate,
    Not
  };

  explicit UnaryOperator(frontend::SourceRange const &range, Kind kind,
                         std::unique_ptr<Expression> operand)
      : Expression(range), operand_(std::move(operand)), kind_(kind) {}
  ICARUS_AST_VIRTUAL_METHODS;

  Kind kind() const { return kind_; }
  Expression const *operand() const { return operand_.get(); }

 private:
  std::unique_ptr<Expression> operand_;
  Kind kind_;
};

// ConditionalGoto:
// Represents a statement describing where a block should jump after completion.
//
// Example (in context of a scope):
//  ```
//  while ::= scope {
//    init ::= jump(b: bool) {
//      goto b, do() | exit()
//    }
//    do ::= block {
//      before ::= () -> () {}
//      after ::= jump() { goto start() }
//    }
//    done ::= () -> () {}
//  }
//  ```
//
//  Note: We generally try to keep these alphabetical, but in this case, the
//  body depends on `Identifier`.
struct ConditionalGoto : Node {
  explicit ConditionalGoto(frontend::SourceRange const &range,
                           std::unique_ptr<Expression> condition,
                           std::vector<std::unique_ptr<Call>> true_calls,
                           std::vector<std::unique_ptr<Call>> false_calls)
      : Node(range), condition_(std::move(condition)) {
    for (auto &call : true_calls) {
      auto [callee, ordered_args] = std::move(*call).extract();
      if (auto *id = callee->if_as<Identifier>()) {
        true_options_.emplace_back(std::string{id->name()},
                                   std::move(ordered_args).DropOrder());
      } else {
        UNREACHABLE();
      }
    }

    for (auto &call : false_calls) {
      auto [callee, ordered_args] = std::move(*call).extract();
      if (auto *id = callee->if_as<Identifier>()) {
        false_options_.emplace_back(std::string{id->name()},
                                    std::move(ordered_args).DropOrder());
      } else {
        UNREACHABLE();
      }
    }
  }

  ICARUS_AST_VIRTUAL_METHODS;

  Expression const *condition() const { return condition_.get(); }
  absl::Span<JumpOption const> true_options() const { return true_options_; }
  absl::Span<JumpOption const> false_options() const { return false_options_; }

 private:
  // A jump will evaluate at compile-time to the first option for which the
  // scope node has all possible blocks.
  std::unique_ptr<ast::Expression> condition_;
  std::vector<JumpOption> true_options_, false_options_;
};

// UnconditionalGoto:
// Represents a statement describing where a block should jump after completion.
//
// Example (in context of a scope):
//  ```
//  forever ::= scope {
//    init ::= jump(b: bool) {
//      goto do()
//    }
//    do ::= block {
//      before ::= () -> () {}
//      after ::= jump() { goto start() }
//    }
//    done ::= () -> () {}
//  }
//  ```
//
//  Note: We generally try to keep these alphabetical, but in this case, the
//  body depends on `Identifier`.
struct UnconditionalGoto : Node {
  explicit UnconditionalGoto(frontend::SourceRange const &range,
                             std::vector<std::unique_ptr<Call>> calls)
      : Node(range) {
    for (auto &call : calls) {
      auto [callee, ordered_args] = std::move(*call).extract();
      if (auto *id = callee->if_as<Identifier>()) {
        options_.emplace_back(std::string{id->name()},
                              std::move(ordered_args).DropOrder());
      } else {
        UNREACHABLE();
      }
    }
  }

  ICARUS_AST_VIRTUAL_METHODS;

  absl::Span<JumpOption const> options() const { return options_; }

 private:
  // A jump will evaluate at compile-time to the first option for which the
  // scope node has all possible blocks.
  std::vector<JumpOption> options_;
};

// YieldStmt:
// Represents a yield statement. Arbitrarily many expressions can be passed.
//
// Examples:
//  * `<< "hello", 42`
//  * `#.my_label << "hello", 42`
//  * `#.my_label <<`
//
struct YieldStmt : Node {
  explicit YieldStmt(frontend::SourceRange const &range,
                     std::vector<std::unique_ptr<Expression>> exprs,
                     std::unique_ptr<ast::Label> label = nullptr)
      : Node(range), exprs_(std::move(exprs)), label_(std::move(label)) {}
  base::PtrSpan<Expression const> exprs() const { return exprs_; }

  ast::Label const *label() const { return label_.get(); }

  ICARUS_AST_VIRTUAL_METHODS;

 private:
  std::vector<std::unique_ptr<Expression>> exprs_;
  std::unique_ptr<ast::Label> label_;
};

#undef ICARUS_AST_VIRTUAL_METHODS

inline void Declaration::set_initial_value(std::unique_ptr<Expression> expr) {
  ASSERT(init_val_ == nullptr);
  if (auto const *id = expr->if_as<Identifier>()) {
    if (id->name().empty()) { flags_ |= f_InitIsHole; }
  }
  init_val_ = std::move(expr);
}

}  // namespace ast

#endif  // ICARUS_AST_AST_H
