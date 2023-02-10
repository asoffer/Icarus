#ifndef ICARUS_SEMANTIC_ANALYSIS_CONTEXT_H
#define ICARUS_SEMANTIC_ANALYSIS_CONTEXT_H

#include "absl/container/flat_hash_map.h"
#include "absl/container/node_hash_map.h"
#include "ast/ast.h"
#include "module/symbol.h"
#include "semantic_analysis/instruction_set.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

// `Context` is responsible for holding all information computed during the
// compilation process of a single module.
struct Context {
  // Returns the qualified types associated with `expr`. Requires that `expr`
  // already have some qualified types associated with it on this `Context`.
  std::span<QualifiedType const> qualified_types(
      ast::Expression const *expr) const;

  // Returns the qualified type associated with `expr`. Requires that `expr`
  // already have exactly one qualified types associated with it on this
  // `Context`.
  QualifiedType qualified_type(ast::Expression const *expr) const;

  // Sets the qualified types associated with `expr` to be `qualified_types`.
  // Requires that `expr` does not yet have any qualified types associated with
  // it on this `Context.`
  std::span<QualifiedType const> set_qualified_types(
      ast::Expression const *expr, std::vector<QualifiedType> qualified_types);

  // Sets the qualified types associated with `expr` to be a sequence consisting
  // of just the one value `qualified_type`. Requires that `expr` does not yet
  // have any qualified types associated with it on this `Context.`
  std::span<QualifiedType const> set_qualified_type(
      ast::Expression const *expr, QualifiedType qualified_types);

  // Given a return statement, returns a view of a collection of the qualified
  // types returned from that return statement. Requires that `set_return_types`
  // was previously called with `return_stmt` as an argument.
  std::span<QualifiedType const> return_types(
      ast::ReturnStmt const *return_stmt) const;

  // Given a return statement, and a sequence of qualified types returned from
  // that return statement, stores that association in this `Context`. This
  // member function must not have been previously called with `return_stmt` as
  // an argument.
  void set_return_types(ast::ReturnStmt const *return_stmt,
                        std::vector<QualifiedType> return_types);

  // Inserts space in this `Context` to hold the constant value represented by
  // `expr` if no such space exists. Returns a pair consisting of a pointer to
  // the space associated with the value, and a bool indicating whether the
  // space was inserted (true) or already existed (false).
  std::pair<std::vector<std::byte> *, bool> insert_constant(
      ast::Expression const *expr) {
    auto [iter, inserted] = constants_.try_emplace(expr);
    return std::pair<std::vector<std::byte> *, bool>(&iter->second, inserted);
  }

  // Inserts space in this `Context` and initializes it with the value `t` to
  // represent the evaluation of `expr` if no such space exists. Returns a pair
  // consisting of a pointer to the space associated with the value, and a bool
  // indicating whether the space was inserted (true) or already existed
  // (false).
  template <typename T>
  std::pair<T *, bool> insert_constant(
      ast::Expression const *expr,
      T const &t) requires(std::is_trivially_copyable_v<T>) {
    auto [iter, inserted] = constants_.try_emplace(expr);
    if (inserted) { std::memcpy(std::addressof(t), &iter->second, sizeof(T)); }
    return std::pair<std::vector<std::byte> *, bool>(&iter->second, inserted);
  }

  // Returns a span over the bytes corresponding to the evaluation of the
  // constant expression `expr` in the given context.
  std::span<std::byte const> constant(ast::Expression const *expr) const {
    auto iter = constants_.find(expr);
    ASSERT(iter != constants_.end());
    return iter->second;
  }

  // Returns the value `T` corresponding to the evaluation of the constant
  // `expr` in the given context.
  template <typename T>
  T constant(ast::Expression const *expr) const
      requires(std::is_trivially_copyable_v<T>) {
    T result;
    std::span span = constant(expr);
    ASSERT(sizeof(result) == span.size());
    std::memcpy(&result, span.data(), sizeof(result));
    return result;
  }

  // Represents a unique identifier for a symbol, potentially within a local
  // scope, or across modules boundaries.
  struct alignas(8) TODOStruct {};
  using symbol_ref_type =
      nth::PtrUnion<ast::Declaration::Id const, TODOStruct const>;

  // Given an identifier AST node `id`, and a `symbol_ref_type`, establishes the
  // connection that `id` refers to the symbol `symbol`. Requires that `id` not
  // yet have any established connection to any `symbol_ref_type` on this
  // `Context`.
  void set_symbol(ast::Identifier const *id, symbol_ref_type symbol);

  // Returns the `symbol_ref_type` representing the symbol referred to by `id`.
  // Requires that `set_symbol` be called on `id` beforehand.
  symbol_ref_type symbol(ast::Identifier const *id) const;

  // A data structure responsible for identifying a callable AST node along with
  // all information required to complete its type-verification. During
  // verification of a call-expression, we only verify the parameters of callees
  // before considering viable overloads. This allows us to address situations
  // like the one in this example:
  //
  // ```
  // f ::= (x: i64, y: bool) => f(y, x)
  // f ::= (x: bool, y: i64) => true
  // ```
  //
  // When computing the type of `f` we are in the middle of the type-checking
  // process for a function in the overload set of `f`. In particular, the
  // computation is relevant to the type of that overload. It is not possible to
  // completely evaluate the types of all overloads in `f` before proceeding
  // further. It is however possible to evaluate the types of all parameters.
  // Because parameters (and not return types) are all that can be used during
  // type-verification of call-expressions, this is sufficient to see that the
  // overload referred to in `f(y, x)` must be the second one (and not
  // self-referentially the first one). Once this overload is resolved we need
  // to continue the process of computing the type of the chosen overload.
  struct CallableIdentifier {
    explicit CallableIdentifier(ast::Expression const *expression)
        : entry_(expression) {}

    explicit CallableIdentifier(module::TypedFunction const &fn) : entry_(fn) {}

    ast::Expression const *expression() const {
      auto const *const *ptr = std::get_if<ast::Expression const *>(&entry_);
      return ptr ? *ptr : nullptr;
    }

    module::TypedFunction function() const {
      return std::get<module::TypedFunction>(entry_);
    }

   private:
    std::variant<ast::Expression const *, module::TypedFunction> entry_;
  };

  // Sets the parameter types associated with a `ast::ParameterizedExpression`
  // node that can be used to invoke the expression.
  std::span<absl::flat_hash_map<core::ParameterType,
                                Context::CallableIdentifier> const>
  set_parameters(
      ast::Expression const *expr,
      std::vector<absl::flat_hash_map<core::ParameterType, CallableIdentifier>>
          parameters);

  // Given `node` representing a call expression, returns a reference to the
  // `CallableIdentifier` representing the expression which was identified
  // during overload resoution as the expression being invoked. The member
  // function `set_callee` must have previously been called with `node` as an
  // argument.
  CallableIdentifier const &callee(ast::Call const *node);

  // Associates with `node`, the given `CallableIdentifier` distinguishing which
  // overload is intended to be called. This member function must not have been
  // previously called with `node` as an argument.
  void set_callee(ast::Call const *node, CallableIdentifier const *identifier);

 private:
  absl::flat_hash_map<ast::Expression const *, std::vector<QualifiedType>>
      type_;

  absl::flat_hash_map<
      ast::Expression const *,
      std::vector<absl::flat_hash_map<core::ParameterType, CallableIdentifier>>>
      parameters_;

  absl::flat_hash_map<ast::Call const *, CallableIdentifier const *> callees_;

  absl::flat_hash_map<ast::Identifier const *, symbol_ref_type> symbols_;

  absl::flat_hash_map<ast::ReturnStmt const *, std::vector<QualifiedType>>
      returns_;

  absl::node_hash_map<ast::Expression const *, std::vector<std::byte>>
      constants_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_CONTEXT_H
