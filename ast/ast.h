#ifndef ICARUS_AST_AST_H
#define ICARUS_AST_AST_H

#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <variant>
#include <vector>

#include "absl/numeric/int128.h"
#include "ast/build_param_dependency_graph.h"
#include "ast/declaration.h"
#include "ast/expression.h"
#include "ast/node.h"
#include "ast/scope.h"
#include "base/ptr_span.h"
#include "core/arguments.h"
#include "core/parameters.h"
#include "core/type_system/type.h"
#include "data_types/addr.h"
#include "data_types/label.h"
#include "frontend/lex/operators.h"
#include "nth/meta/sequence.h"
#include "nth/meta/type.h"
#include "semantic_analysis/type_system.h"

namespace ast {

// WithScope:
// A mixin which adds a scope of the given type `S`.
struct WithScope {
  explicit WithScope(Scope::Kind kind) : body_scope_(kind) {}

  Scope const &body_scope() const { return body_scope_; }
  Scope &body_scope() { return body_scope_; }

 protected:
  Scope body_scope_;
};

// Access:
// Represents member access with the `.` operator.
//
// Examples:
//  * `my_pair.first_element`
//  * `(some + computation).member`
//
struct Access : Expression {
  explicit Access(std::string_view range, size_t length,
                  std::unique_ptr<Expression> operand)
      : Expression(IndexOf<Access>(), range),
        operand_(std::move(operand)),
        member_name_length_(length) {}
  constexpr std::string_view member_name() const { return member_range(); }
  constexpr std::string_view member_range() const {
    return std::string_view(range().end() - member_name_length_, range().end());
  }
  Expression const *operand() const { return operand_.get(); }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  std::unique_ptr<Expression> operand_;
  size_t member_name_length_;
};

// ArgumentType:
// Represents the type of the argument bound to a parameter of the given name in
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
  explicit ArgumentType(std::string_view range, char const *name_start)
      : Expression(IndexOf<ArgumentType>(), range), name_start_(name_start) {}
  std::string_view name() const {
    return std::string_view(name_start_, range().end());
  }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  char const *name_start_;
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
  ArrayLiteral(std::string_view range,
               std::vector<std::unique_ptr<Expression>> elements)
      : Expression(IndexOf<ArrayLiteral>(), range),
        elements_(std::move(elements)) {}
  bool empty() const { return elements_.empty(); }
  size_t size() const { return elements_.size(); }

  base::PtrSpan<Expression const> elements() const { return elements_; }

  auto extract() && { return std::move(elements_); }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  std::vector<std::unique_ptr<Expression>> elements_;
};

// Assignment:
// Represents an assignment of one or more values to one or more references.
// The number of values must match the number of references.
//
// Examples:
// * `a = b`
// * `(a, b) = (c, d)`
//
struct Assignment : Node {
  explicit Assignment(std::string_view range,
                      std::vector<std::unique_ptr<Expression>> lhs,
                      std::vector<std::unique_ptr<Expression>> rhs)
      : Node(IndexOf<Assignment>(), range),
        lhs_(std::move(lhs)),
        rhs_(std::move(rhs)) {}
  base::PtrSpan<Expression const> lhs() const { return lhs_; }
  base::PtrSpan<Expression const> rhs() const { return rhs_; }

  auto extract() && { return std::pair(std::move(lhs_), std::move(rhs_)); }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

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
//  * `[5; i32]`     ... Represnts the type of an array that can hold five
//                         32-bit integers
//  * `[0; bool]`      ... Represents the type of an array that can hold zero
//                         booleans.
//  * `[3; [2; i8]]` ... Represents the type of an array that can hold three
//                         elements, each of which is an array that can hold two
//                         8-bit integers.
//  * `[3, 2; i8]`   ... A shorthand syntax for `[3; [2; i8]]`
struct ArrayType : Expression {
  explicit ArrayType(std::string_view range, std::unique_ptr<Expression> length,
                     std::unique_ptr<Expression> data_type)
      : Expression(IndexOf<ArrayType>(), range),
        data_type_(std::move(data_type)) {
    lengths_.push_back(std::move(length));
  }
  ArrayType(std::string_view range,
            std::vector<std::unique_ptr<Expression>> lengths,
            std::unique_ptr<Expression> data_type)
      : Expression(IndexOf<ArrayType>(), range),
        lengths_(std::move(lengths)),
        data_type_(std::move(data_type)) {}
  base::PtrSpan<Expression const> lengths() const { return lengths_; }
  Expression const &length(size_t i) const { return *lengths_[i]; }
  Expression const &data_type() const { return *data_type_; }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

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
  enum class Kind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    SymbolAnd,
    SymbolOr,
    SymbolXor,
    BlockJump,
  };

  static std::string_view Symbol(Kind k) {
    constexpr std::array<std::string_view, 12> kSymbols{
        "+", "-", "*", "/", "%", "and", "or", "xor", "&", "|", "^", ">>"};
    return kSymbols[static_cast<int>(k)];
  }

  explicit BinaryOperator(std::unique_ptr<Expression> lhs, Kind kind,
                          std::unique_ptr<Expression> rhs)
      : Expression(IndexOf<BinaryOperator>(),
                   std::string_view(lhs->range().begin(), rhs->range().end())),
        kind_(kind),
        lhs_(std::move(lhs)),
        rhs_(std::move(rhs)) {}
  Expression const &lhs() const { return *lhs_; }
  Expression const &rhs() const { return *rhs_; }
  Kind kind() const { return kind_; }

  std::string_view operator_range() const {
    auto start = lhs().range().end();
    auto end   = rhs().range().rbegin();
    while (std::isspace(*start)) { ++start; }
    while (std::isspace(*end)) { ++end; }
    return std::string_view(start, end.base());
  }

  auto extract() && { return std::pair{std::move(lhs_), std::move(rhs_)}; }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  Kind kind_;
  std::unique_ptr<Expression> lhs_, rhs_;
};

// BinaryAssignmentOperator:
// Represents a call to a binary assignment operator.
//
// Examples:
//  * `x += y`
//  * `n *= m`
//
struct BinaryAssignmentOperator : BinaryOperator {
  static std::string_view Symbol(Kind k) {
    constexpr std::array<std::string_view, 11> kSymbols{
        "+=", "-=", "*=", "/=", "%=", "", "", "", "&=", "|=", "^"};
    return kSymbols[static_cast<int>(k)];
  }

  explicit BinaryAssignmentOperator(std::unique_ptr<Expression> lhs, Kind kind,
                                    std::unique_ptr<Expression> rhs)
      : BinaryOperator(std::move(lhs), kind, std::move(rhs)) {
    which_ = IndexOf<BinaryAssignmentOperator>();
  }
  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;
};

// ParameterizedExpression:
// This is a parent-class for all nodes that have parameters, allowing us to
// handle those parameters uniformly. oreover, this gives us the ability to key
// hash-tables on `ParameterizedExpression const *`.
struct ParameterizedExpression : Expression {
  explicit ParameterizedExpression(int8_t which, std::string_view range)
      : Expression(which, range) {}

  explicit ParameterizedExpression(int8_t which, std::string_view range,
                                   std::vector<Declaration> params)
      : Expression(which, range) {
    for (auto &param : params) {
      // NOTE: It's safe to save a `std::string_view` to a parameter because
      // both the declaration and this will live for the length of the syntax
      // tree.
      //
      NTH_ASSERT(param.ids().size() == 1u);
      parameters_.append(std::string(param.ids()[0].name()), std::move(param));
    }
  }

  core::Parameters<Declaration> const &parameters() const {
    return parameters_;
  }

  // Returns a sequence of (parameter-index, dependency-node) pairs ordered in
  // such a way that each has no dependencies on any that come after it.
  std::span<std::pair<int, core::DependencyNode<Declaration>> const>
  ordered_dependency_nodes() const {
    return ordered_dependency_nodes_;
  }

  // Returns true if the expression accepts a generic parameter (i.e., a
  // constant parameter or a parameter with a deduced type).
  constexpr bool is_generic() const { return is_generic_; }

 protected:
  void InitializeParams();

  core::Parameters<Declaration> parameters_;
  std::vector<std::pair<int, core::DependencyNode<Declaration>>>
      ordered_dependency_nodes_;
  bool is_generic_ = false;
};

// PatternMatch:
//
// Represents a pattern matching expression, which may or may not own the
// expression being matched against.
//
// Examples:
// ```
// 3 ~ 2 * `N + 1
// identity ::= (x: ~`x) => x
// ```
//
struct PatternMatch : Expression {
  explicit PatternMatch(std::unique_ptr<Expression> expr_to_match,
                        std::unique_ptr<Expression> pattern)
      : Expression(IndexOf<PatternMatch>(),
                   std::string_view(expr_to_match->range().begin(),
                                    pattern->range().end())),
        expr_to_match_(reinterpret_cast<uintptr_t>(expr_to_match.release()) |
                       uintptr_t{1}),
        pattern_(std::move(pattern)) {}
  explicit PatternMatch(std::string_view range,
                        std::unique_ptr<Expression> pattern)
      : Expression(IndexOf<PatternMatch>(), range),
        expr_to_match_(0),
        pattern_(std::move(pattern)) {}

  ~PatternMatch() override {
    if (is_binary()) { delete &expr(); }
  }

  Expression const &expr() const {
    return *NTH_ASSERT_NOT_NULL(
        reinterpret_cast<Expression const *>(expr_to_match_ & ~uintptr_t{1}));
  }

  Expression const &pattern() const { return *NTH_ASSERT_NOT_NULL(pattern_.get()); }

  bool is_binary() const { return expr_to_match_ & uintptr_t{1}; }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  Expression &expr() {
    return *NTH_ASSERT_NOT_NULL(
        reinterpret_cast<Expression *>(expr_to_match_ & ~uintptr_t{1}));
  }

  void set_match_against(Declaration *d) {
    expr_to_match_ = reinterpret_cast<uintptr_t>(d) | uintptr_t{is_binary()};
  }

  uintptr_t expr_to_match_;
  std::unique_ptr<Expression> pattern_;
};

// BindingDeclaration:
// Represents a pattern matching binding declaration.
//
// Examples:
// * `N
// * sortable`Container
struct BindingDeclaration : Declaration {
  explicit BindingDeclaration(std::string_view range, Declaration::Id id,
                              std::unique_ptr<Expression> constraint = nullptr)
      : Declaration(range, ToVector(std::move(id)), nullptr, nullptr,
                    f_IsConst),
        constraint_(std::move(constraint)) {
    which_ = IndexOf<BindingDeclaration>();
  }

  PatternMatch const &pattern() const { return *NTH_ASSERT_NOT_NULL(pattern_); }
  Expression const *constraint() const { return constraint_.get(); }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  static std::vector<ast::Declaration::Id> ToVector(ast::Declaration::Id id) {
    std::vector<ast::Declaration::Id> ids;
    ids.push_back(std::move(id));
    return ids;
  }

  PatternMatch const *pattern_;
  std::unique_ptr<Expression> constraint_;
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
  DesignatedInitializer(std::string_view range,
                        std::unique_ptr<Expression> type,
                        std::vector<std::unique_ptr<Assignment>> assignments)
      : Expression(IndexOf<DesignatedInitializer>(), range),
        type_(std::move(type)),
        assignments_(std::move(assignments)) {
    for (auto const *assignment : this->assignments()) {
      for (auto const *expr : assignment->lhs()) {
        NTH_ASSERT(expr->is<Identifier>());
      }
    }
  }

  Expression const *type() const { return type_.get(); }
  base::PtrSpan<Assignment const> assignments() const { return assignments_; }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  std::unique_ptr<Expression> type_;
  std::vector<std::unique_ptr<Assignment>> assignments_;
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
// (likely in the form of `core::Arguments<std::unique_ptr<Expression>>`).
struct BlockNode : ParameterizedExpression, WithScope {
  explicit BlockNode(std::string_view range, char const *name_end,
                     std::vector<std::unique_ptr<Node>> stmts)
      : ParameterizedExpression(IndexOf<BlockNode>(), range),
        WithScope(Scope::Kind::Executable),
        name_end_(name_end),
        stmts_(std::move(stmts)) {}
  explicit BlockNode(std::string_view range, char const *name_end,
                     std::vector<Declaration> params,
                     std::vector<std::unique_ptr<Node>> stmts)
      : ParameterizedExpression(IndexOf<BlockNode>(), range, std::move(params)),
        WithScope(Scope::Kind::Executable),
        name_end_(name_end),
        stmts_(std::move(stmts)) {
    // TODO: We only track that this is a block parameter because arguments
    // bound to these parameters end up being stored on the stack and we need to
    // make sure we insert the correct load instructions. There should be a more
    // cohesive way to handle this and function parameters simultaneously.
    for (auto &param : parameters_) {
      param.value.flags() |= Declaration::f_IsBlockParam;
    }
  }
  BlockNode(BlockNode &&) noexcept            = default;
  BlockNode &operator=(BlockNode &&) noexcept = default;

  std::string_view name() const { return name_range(); }
  base::PtrSpan<Node const> stmts() const { return stmts_; }
  ScopeNode const *parent() const { return parent_; }

  std::string_view name_range() const {
    return std::string_view(range().begin(), name_end_);
  }

  std::vector<std::unique_ptr<Node>> extract() && { return std::move(stmts_); }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  friend struct ScopeNode;
  char const *name_end_;
  std::vector<std::unique_ptr<Node>> stmts_;
  ScopeNode *parent_ = nullptr;
};

// Call:
// Represents a function call, or call to any other callable object.
//
// Examples:
//  * `f(a, b, c = 3)`
//  * `arg'func`
struct Call : Expression {
  struct Argument {
    explicit Argument(std::string_view name, std::unique_ptr<Expression> expr)
        : name_(name), expr_(std::move(expr)) {}

    bool named() const { return not name_.empty(); }
    std::string_view name() const { return name_; }
    ast::Expression const &expr() const { return *expr_; }
    ast::Expression &expr() { return *expr_; }

    std::pair<std::string_view, std::unique_ptr<ast::Expression>> extract() && {
      return std::pair(name_, std::move(expr_));
    }

   private:
    std::string_view name_;
    std::unique_ptr<Expression> expr_;
  };

  explicit Call(std::string_view range, std::unique_ptr<Expression> callee,
                std::vector<Argument> arguments, size_t prefix_split)
      : Expression(IndexOf<Call>(), range),
        callee_(std::move(callee)),
        arguments_(std::move(arguments)),
        prefix_split_(prefix_split) {
    size_t i = 0;
    for (auto const &arg : arguments_) {
      if (arg.named()) { break; }
      ++i;
    }
    positional_split_ = i;
  }
  Expression const *callee() const { return callee_.get(); }

  std::span<Argument const> prefix_arguments() const {
    return std::span<Argument const>(arguments_.begin(),
                                     arguments_.begin() + prefix_split_);
  }

  std::span<Argument const> postfix_arguments() const {
    return std::span<Argument const>(arguments_.begin() + prefix_split_,
                                     arguments_.end());
  }

  std::span<Argument const> arguments() const { return arguments_; }

  std::span<Argument const> named_arguments() const {
    return std::span<Argument const>(arguments_.begin() + positional_split_,
                                     arguments_.end());
  }

  std::span<Argument const> positional_arguments() const {
    return std::span<Argument const>(arguments_.begin(),
                                     arguments_.begin() + positional_split_);
  }

  auto extract() && {
    return std::make_tuple(std::move(callee_), std::move(arguments_));
  }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  std::unique_ptr<Expression> callee_;
  std::vector<Argument> arguments_;
  size_t positional_split_, prefix_split_;
};

// Cast:
// Represents a type-conversion. These can be either builtin conversion (for
// example, between integral types) or user-defined conversion via overloading
// the `as` operator. In either case, syntactically, they are represented by
// `<expr> as <type-expr>`.
//
// Examples:
//  * `3 as u32`
//  * `null as *i64`
struct Cast : Expression {
  explicit Cast(std::string_view range, std::unique_ptr<Expression> expr,
                std::unique_ptr<Expression> type_expr)
      : Expression(IndexOf<Cast>(), range),
        expr_(std::move(expr)),
        type_(std::move(type_expr)) {}
  Expression const *expr() const { return expr_.get(); }
  Expression const *type() const { return type_.get(); }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

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
  explicit ComparisonOperator(std::string_view range,
                              std::unique_ptr<Expression> expr)
      : Expression(IndexOf<ComparisonOperator>(), range) {
    exprs_.push_back(std::move(expr));
  }

  void append(frontend::Operator op, std::unique_ptr<Expression> expr) {
    ops_.push_back(op);
    exprs_.push_back(std::move(expr));
  }

  base::PtrSpan<Expression const> exprs() const { return exprs_; }
  std::span<frontend::Operator const> ops() const { return ops_; }

  // Returns a source range consisting of the `i`th (zero-indexed) operator and
  // surrounding expressions in this chain of comparison operators.
  std::string_view binary_range(size_t i) const {
    return std::string_view(exprs_[i]->range().begin(),
                            exprs_[i + 1]->range().end());
  }

  auto extract() && { return std::move(exprs_); }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

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
struct EnumLiteral : Expression, WithScope {
  enum Kind : char { Enum, Flags };

  EnumLiteral(
      std::string_view range, std::vector<std::string_view> enumerators,
      absl::flat_hash_map<std::string_view, std::unique_ptr<Expression>> values,
      Kind kind)
      : Expression(IndexOf<EnumLiteral>(), range),
        WithScope(Scope::Kind::Declarative),
        enumerators_(std::move(enumerators)),
        values_(std::move(values)),
        kind_(kind) {}
  std::span<std::string_view const> enumerators() const { return enumerators_; }
  absl::flat_hash_map<std::string_view, std::unique_ptr<Expression>> const &
  specified_values() const {
    return values_;
  }
  Kind kind() const { return kind_; }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  std::vector<std::string_view> enumerators_;
  absl::flat_hash_map<std::string_view, std::unique_ptr<Expression>> values_;
  Kind kind_;
};

// FunctionLiteral:
//
// Represents a literal function. Functions may have deduced return-type, which
// can be determined by calling `outputs()`, If the result is std::nullopt, the
// output is deduced. Functions may be generic.
//
// Examples:
// * `(n: i32) -> () { print n }`
// * `() -> i32 { return 3 }`
// * `(n: i32, m: i32) => n * m`
// * `(T :: type, val: T) => val`
//
struct FunctionLiteral : ParameterizedExpression, WithScope {
  explicit FunctionLiteral(
      std::string_view range, std::vector<Declaration> in_params,
      std::vector<std::unique_ptr<Node>> stmts,
      std::optional<std::vector<std::unique_ptr<Expression>>> out_params =
          std::nullopt)
      : ParameterizedExpression(IndexOf<FunctionLiteral>(), range,
                                std::move(in_params)),
        WithScope(Scope::Kind::BoundaryExecutable),
        outputs_(std::move(out_params)),
        stmts_(std::move(stmts)) {}
  base::PtrSpan<Node const> stmts() const { return stmts_; }

  std::optional<base::PtrSpan<Expression const>> outputs() const {
    if (not outputs_) { return std::nullopt; }
    return *outputs_;
  }

  absl::flat_hash_set<ReturnStmt const *> const &returns() const {
    return returns_;
  }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  friend ReturnStmt;
  std::optional<std::vector<std::unique_ptr<Expression>>> outputs_;
  std::vector<std::unique_ptr<Node>> stmts_;

  absl::flat_hash_set<ReturnStmt const *> returns_;
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
// * `i64 -> bool`
// * `(b: bool) -> ()`
// * `(n: i64, b: bool) -> (f32, f32)
//
struct FunctionType : Expression {
  FunctionType(std::string_view range,
               std::vector<std::unique_ptr<Expression>> params,
               std::vector<std::unique_ptr<Expression>> output)
      : Expression(IndexOf<FunctionType>(), range),
        parameters_(std::move(params)),
        output_(std::move(output)) {}
  base::PtrSpan<Expression const> parameters() const { return parameters_; }
  base::PtrSpan<Expression const> outputs() const { return output_; }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

  auto extract() && {
    return std::pair(std::move(parameters_), std::move(output_));
  }

 private:
  std::vector<std::unique_ptr<Expression>> parameters_;
  std::vector<std::unique_ptr<Expression>> output_;
};

// Identifier:
// Represents any user-defined identifier.
struct Identifier : Expression {
  Identifier(std::string_view range)
      : Expression(IndexOf<Identifier>(), range) {}

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

  std::string_view name() const { return range_; }
};

// Import:
// Represents a request from one module to use parts of a different module.
//
// Examples:
//  * `import "a_module.ic"`
//  * `import function_returning_a_string()`
struct Import : Expression {
  explicit Import(std::string_view range, std::unique_ptr<Expression> expr)
      : Expression(IndexOf<Import>(), range), operand_(std::move(expr)) {}

  Expression const *operand() const { return operand_.get(); }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

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
  explicit Index(std::string_view range, std::unique_ptr<Expression> lhs,
                 std::unique_ptr<Expression> rhs)
      : Expression(IndexOf<Index>(), range),
        lhs_(std::move(lhs)),
        rhs_(std::move(rhs)) {}

  Expression const *lhs() const { return lhs_.get(); }
  Expression const *rhs() const { return rhs_.get(); }
  auto extract() && { return std::pair(std::move(lhs_), std::move(rhs_)); }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  std::unique_ptr<Expression> lhs_, rhs_;
};

// InterfaceLiteral:
// Represents indexing into an array, buffer-pointer, or a call to the ([])
// operator if/when that may be overloaded.
//
// Examples:
// ```
// interface [T] {}
//
// interface [T] {
//   run :: builtin.callable()
// }
// ```
struct InterfaceLiteral : Expression, WithScope {
  explicit InterfaceLiteral(
      std::string_view range, Declaration::Id identifier,
      absl::flat_hash_map<std::string_view, std::unique_ptr<Expression>>
          members)
      : Expression(IndexOf<InterfaceLiteral>(), range),
        WithScope(Scope::Kind::Declarative),
        context_decl_(ContextDeclaration(std::move(identifier))),
        identifier_(identifier),
        members_(std::move(members)) {}

  Declaration const &context() const { return *context_decl_; }

  absl::flat_hash_map<std::string_view, std::unique_ptr<Expression>> const &
  members() const {
    return members_;
  }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  static std::unique_ptr<Declaration> ContextDeclaration(
      Declaration::Id context_identifier) {
    auto range = context_identifier.range();
    return std::make_unique<Declaration>(
        range, std::vector<Declaration::Id>{std::move(context_identifier)},
        std::make_unique<Terminal>(range, semantic_analysis::Type), nullptr,
        Declaration::f_IsFnParam);
  }

  std::unique_ptr<Declaration> context_decl_;
  Declaration::Id identifier_;
  absl::flat_hash_map<std::string_view, std::unique_ptr<Expression>> members_;
};

// Label:
// Represents a label which can be the target of a yield statement in a scope.
// Other languages used labels for "labelled-break", and this is a similar idea.
//
// Example:
// `#.my_label`
struct Label : Expression {
  explicit Label(std::string_view range, std::string label)
      : Expression(IndexOf<Label>(), range), label_(std::move(label)) {}

  data_types::Label value() const { return data_types::Label(&label_); }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  std::string label_;
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
// struct (N: i64) {
//   val: i64 = N
//   square ::= N * N
// }
// ```
struct ParameterizedStructLiteral : ParameterizedExpression, WithScope {
  ParameterizedStructLiteral(std::string_view range,
                             std::vector<Declaration> params,
                             std::vector<Declaration> fields)
      : ParameterizedExpression(IndexOf<ParameterizedStructLiteral>(), range,
                                std::move(params)),
        WithScope(Scope::Kind::Declarative),
        fields_(std::move(fields)) {}

  std::span<Declaration const> fields() const { return fields_; }

  ParameterizedStructLiteral &operator=(
      ParameterizedStructLiteral &&) noexcept = default;

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

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
  explicit ReturnStmt(std::string_view range,
                      std::vector<std::unique_ptr<Expression>> exprs = {})
      : Node(IndexOf<ReturnStmt>(), range), exprs_(std::move(exprs)) {}

  ast::FunctionLiteral const &function_literal() const {
    return *NTH_ASSERT_NOT_NULL(function_literal_);
  }

  base::PtrSpan<Expression const> exprs() const { return exprs_; }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  // Pointer to the function literal containing this return statement.
  FunctionLiteral *function_literal_;

  // Expressions being returned.
  std::vector<std::unique_ptr<Expression>> exprs_;
};

// Terminal:
// Represents any node that is not an identifier but has no sub-parts. These are
// typically numeric literals, or expressions that are also keywords such as
// `true`, `false`, or `null`.
struct Terminal : Expression {
 private:
  static constexpr auto types =
      nth::type_sequence<bool, data_types::Char, absl::int128, double,
                         data_types::addr_t, core::Type, std::string>;

  using variant_type = nth::type_t<types.reduce(
      [](auto... ts) { return nth::type<std::variant<nth::type_t<ts>...>>; })>;

 public:
  explicit Terminal(std::string_view range, variant_type value)
      : Expression(IndexOf<Terminal>(), range), value_(std::move(value)) {}

  template <typename T>
  T const &value() const {
    return std::get<T>(value_);
  }

  nth::TypeId type() const {
    static auto Types = types.reduce([](auto... ts) {
      return std::array<nth::TypeId, sizeof...(ts)>{ts...};
    });
    return Types[value_.index()];
  };

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  variant_type value_;
};

// ScopeLiteral:
// Represents the definition of a user-defined scope.
//
// Example:
//  ```
//  repeat ::= scope [context] (n: i64) {
//    i := 0
//    while (i < n) do {
//      context.do <<
//      i += 1
//    }
//  }
//  ```
struct ScopeLiteral : ParameterizedExpression, WithScope {
  explicit ScopeLiteral(std::string_view range,
                        Declaration::Id context_identifier,
                        std::vector<Declaration> params,
                        std::vector<std::unique_ptr<Node>> stmts)
      : ParameterizedExpression(IndexOf<ScopeLiteral>(), range,
                                std::move(params)),
        WithScope(Scope::Kind::BoundaryExecutable),
        context_decl_(ContextDeclaration(std::move(context_identifier))),
        stmts_(std::move(stmts)) {}

  Declaration const &context() const { return *context_decl_; }
  base::PtrSpan<Node const> stmts() const { return stmts_; }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  static std::unique_ptr<Declaration> ContextDeclaration(
      Declaration::Id context_identifier) {
    auto range = context_identifier.range();
    NTH_UNIMPLEMENTED();
    // return std::make_unique<Declaration>(
    //     range, std::vector<Declaration::Id>{std::move(context_identifier)},
    //     std::make_unique<Terminal>(range, type::ScopeContext), nullptr,
    //     Declaration::f_IsConst | Declaration::f_IsFnParam);
  }

  std::unique_ptr<Declaration> context_decl_;
  std::vector<std::unique_ptr<Node>> stmts_;
};

// ScopeNode:
//
// Represents the usage of a user-defined scope.
//
// Examples:
//  ```
//  for (0, 100) do [i: i64] {
//    do_something(i)
//  }
//  ```
//
//  `unwrap (maybe_object) or { return -1 }`
//
struct ScopeNode : Expression {
  ScopeNode(std::string_view range, std::unique_ptr<Expression> name,
            std::vector<Call::Argument> args, std::vector<BlockNode> blocks)
      : Expression(IndexOf<ScopeNode>(), range),
        name_(std::move(name)),
        args_(std::move(args)),
        blocks_(WithThisAsParent(std::move(blocks))) {}

  Expression const *name() const { return name_.get(); }
  std::span<Call::Argument const> arguments() const { return args_; }

  std::span<BlockNode const> blocks() const { return blocks_; }

  Label const *label() const { return label_ ? &*label_ : nullptr; }
  void set_label(Label label) { label_ = std::move(label); }

  absl::flat_hash_set<YieldStmt const *> const &yields() const {
    return yields_;
  }

  // Appends the given block not necessarily to this ScopeNode, but to the scope
  // that makes sense syntactically. For instance, in the first example above,
  // the inner `if` ScopeNode checking `condition2` would be appended to.
  void append_block_syntactically(
      BlockNode block, ScopeNode *updated_last_scope_node = nullptr) {
    auto *scope_node = (last_scope_node_ ? last_scope_node_ : this);
    block.parent_    = scope_node;
    scope_node->blocks_.push_back(std::move(block));
    if (updated_last_scope_node) { last_scope_node_ = updated_last_scope_node; }
  }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  friend YieldStmt;

  std::vector<BlockNode> WithThisAsParent(std::vector<BlockNode> &&blocks) {
    for (auto &b : blocks) { b.parent_ = this; }
    return std::move(blocks);
  }

  ScopeNode const *parent_;
  std::optional<Label> label_;
  std::unique_ptr<Expression> name_;
  std::vector<Call::Argument> args_;
  std::vector<BlockNode> blocks_;
  absl::flat_hash_set<YieldStmt const *> yields_;
  ScopeNode *last_scope_node_ = nullptr;
};

// SliceType:
// Represents the syntactic construction for expressing the type of a slice,
// which is a reference to a contiguous buffer of homogenous elements.
//
// Examples:
//  `i32[]`  ... Represnts a slice of 32-bit itnegers
//
struct SliceType : Expression {
  explicit SliceType(std::string_view range,
                     std::unique_ptr<Expression> data_type)
      : Expression(IndexOf<SliceType>(), range),
        data_type_(std::move(data_type)) {}

  Expression const &data_type() const { return *data_type_; }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  std::unique_ptr<Expression> data_type_;
};

// ShortFunctionLiteral:
//
// Represents a literal function which is syntactically "short". That is, it
// uses `=>`, has it's return type inferred and has its body consist of a single
// expression.
//
// Examples:
// * `(n: i32, m: i32) => n * m`
// * `(T :: type, val: T) => val`
// * `(x: $x) => x`
//
struct ShortFunctionLiteral : ParameterizedExpression, WithScope {
  explicit ShortFunctionLiteral(std::string_view range,
                                std::vector<Declaration> params,
                                std::unique_ptr<Expression> body)
      : ParameterizedExpression(IndexOf<ShortFunctionLiteral>(), range,
                                std::move(params)),
        WithScope(Scope::Kind::BoundaryExecutable),
        body_(std::move(body)) {}
  Expression const *body() const { return body_.get(); }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

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
//   x: f64
//   y: f64
// }
//
// struct {}
// ```
struct StructLiteral : Expression, WithScope {
  explicit StructLiteral(std::string_view range,
                         std::vector<Declaration> fields)
      : Expression(IndexOf<StructLiteral>(), range),
        WithScope(Scope::Kind::Declarative),
        fields_(std::move(fields)) {}

  std::span<Declaration const> fields() const { return fields_; }

  StructLiteral &operator=(StructLiteral &&) noexcept = default;

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  std::vector<Declaration> fields_;
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
    Init,
    Copy,
    Move,
    Destroy,
    BufferPointer,
    TypeOf,
    At,
    Pointer,
    Address,
    Negate,
    Not,
    BlockJump,
  };

  explicit UnaryOperator(std::string_view range, Kind kind,
                         std::unique_ptr<Expression> operand)
      : Expression(IndexOf<UnaryOperator>(), range),
        operand_(std::move(operand)),
        kind_(kind) {}
  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

  Kind kind() const { return kind_; }
  Expression const *operand() const { return operand_.get(); }

 private:
  std::unique_ptr<Expression> operand_;
  Kind kind_;
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
  explicit YieldStmt(std::string_view range, std::vector<Call::Argument> args,
                     std::unique_ptr<Label> label = nullptr)
      : Node(IndexOf<YieldStmt>(), range),
        args_(std::move(args)),
        label_(std::move(label)) {}

  std::span<Call::Argument const> arguments() const { return args_; }
  Label const *label() const { return label_.get(); }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  std::vector<Call::Argument> args_;
  std::unique_ptr<Label> label_;
};

// IfStmt:
// Represents an if-statement, which evaluates the condition and then either
// executes the `then` body if the condition is `true`, or the `else` body (if
// it exists and) the condition is `false`.
//
// Examples:
// ```
// if (condition) then { x() } else { y() }
// if (condition) then { z() }
// ```
struct IfStmt : Expression {
  explicit IfStmt(std::string_view range, std::unique_ptr<Expression> condition,
                  std::vector<std::unique_ptr<Node>> true_block)
      : Expression(IndexOf<IfStmt>(), range),
        condition_(std::move(condition)),
        true_block_(std::move(true_block)),
        has_false_block_(false),
        last_if_(this) {}

  void SetFalseBlock(std::vector<std::unique_ptr<Node>> false_block) {
    NTH_ASSERT(not last_if_->has_false_block_);
    last_if_->false_block_     = std::move(false_block);
    last_if_->has_false_block_ = true;
    last_if_                   = nullptr;
  }

  // Appends the given block not necessarily to this ScopeNode, but to the scope
  // that makes sense syntactically. For instance, in the first example above,
  // the inner `if` ScopeNode checking `condition2` would be appended to.
  void AppendElseBlock(std::unique_ptr<ast::IfStmt> node) {
    auto *ptr = node.get();
    NTH_ASSERT(not last_if_->has_false_block_);
    last_if_->false_block_.push_back(std::move(node));
    last_if_->has_false_block_ = true;
    last_if_                   = ptr;
  }

  Expression const &condition() const { return *condition_; }
  base::PtrSpan<Node const> true_block() const { return true_block_; }
  base::PtrSpan<Node const> false_block() const { return false_block_; }

  Scope const &true_scope() const { return true_scope_; }
  Scope const &false_scope() const { return false_scope_; }

  bool has_false_block() const { return has_false_block_; }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  std::unique_ptr<Expression> condition_;
  std::vector<std::unique_ptr<Node>> true_block_;
  std::vector<std::unique_ptr<Node>> false_block_;
  Scope true_scope_{Scope::Kind::Executable};
  Scope false_scope_{Scope::Kind::Executable};

  bool has_false_block_;
  IfStmt *last_if_ = nullptr;
};

// WhileStmt:
// Represents a while-loop, which repeatedly executes the body while the
// condition evaluates to `true`.
//
// Example:
// ```
// while (condition) do {
//   x()
//   y()
//   z()
// }
// ```
struct WhileStmt : Expression, WithScope {
  explicit WhileStmt(std::string_view range,
                     std::unique_ptr<Expression> condition,
                     std::vector<std::unique_ptr<Node>> body)
      : Expression(IndexOf<WhileStmt>(), range),
        WithScope(Scope::Kind::Executable),
        condition_(std::move(condition)),
        body_(std::move(body)) {}

  Expression const &condition() const { return *condition_; }
  base::PtrSpan<Node const> body() const { return body_; }

  void DebugStrAppend(std::string *out, size_t indent) const override;
  void Initialize(Node::Initializer &initializer) override;

 private:
  std::unique_ptr<Expression> condition_;
  std::vector<std::unique_ptr<Node>> body_;
};

inline void Declaration::set_initial_value(std::unique_ptr<Expression> expr) {
  NTH_ASSERT(init_val_ == nullptr);
  if (auto const *id = expr->if_as<Identifier>()) {
    if (id->name().empty()) { flags_ |= f_InitIsHole; }
  }
  init_val_ = std::move(expr);
}

}  // namespace ast

#endif  // ICARUS_AST_AST_H
