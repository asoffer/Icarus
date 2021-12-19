#ifndef ICARUS_COMPILER_CONTEXT_H
#define ICARUS_COMPILER_CONTEXT_H

#include <forward_list>
#include <memory>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/node_hash_map.h"
#include "absl/hash/hash.h"
#include "ast/ast.h"
#include "base/guarded.h"
#include "compiler/bound_parameters.h"
#include "compiler/jump_map.h"
#include "ir/builder.h"
#include "ir/byte_code/byte_code.h"
#include "ir/compiled_fn.h"
#include "ir/compiled_scope.h"
#include "ir/module.h"
#include "ir/value/reg.h"
#include "ir/value/scope.h"
#include "module/module.h"
#include "type/qual_type.h"

namespace compiler {
namespace internal_context {

template <typename T>
using DefiningAstNodeType = std::conditional_t<
    (base::meta<T> == base::meta<type::Struct>), ast::StructLiteral,
    std::conditional_t<(base::meta<T> == base::meta<type::Enum> or
                        base::meta<T> == base::meta<type::Flags>),
                       ast::EnumLiteral, void>>;

}  // namespace internal_context

struct CompiledModule;

// Context holds all data that the compiler computes about the program by
// traversing the syntax tree. This includes type information, compiled
// functions and jumps, etc. Note that this data may be dependent on constant
// parameters to a function, jump, or struct. To account for such dependencies,
// Context is intrusively a tree. Each Context has a pointer to it's parent
// (except the root whose parent-pointer is null), as well as a map keyed on
// arguments whose values hold child Context.
//
// For instance, the program
// ```
// f ::= (n :: i64) -> () {
//   size ::= n * n
//   array: [size; bool]
//   ...
// }
//
// f(1)
// f(2)
// ```
//
// would have three Context nodes. The root node, which has the other two nodes
// as children. These nodes are keyed on the arguments to `f`, one where `n` is
// 1 and one where `n` is 2. Note that the type of `array` is not available at
// the root node as it's type is dependent on `n`. Rather, on the two child
// nodes it has type `[1; bool]` and `[4; bool]` respectively.  Moreover, even
// the type of `size` (despite always being `i64` is not available on the root
// node. Instead, it is available on all child nodes with the same value of
// `i64`.
//
// Though there is nothing special about recursive instantiations, it's worth
// describing an example as well:
//
// ```
// pow2 ::= (n :: i64) -> i64 {
//   if (n == 0) then {
//     return 1
//   } else {
//     return pow2(n - 1) * 2
//   }
// }
//
// pow2(3)
// ```
//
// In this example, the expression `pow2(3)` instantiates a subcontext of the
// root binding, 3 to `n`. In doing so, it requires instantiating `pow2(2)`
// which becomes another subcontext of the root. This continues on so that the
// end result is that the root context has 4 subcontexts, one binding `n` to
// each of 0, 1, 2, and 3.
//
// The important thing to note here is that the subcontexts are all of the root,
// rather than in a chain. This is because we instantiate subcontexts in the
// context of the callee, not the call-site. In this case, despite there being
// two different call-sites, there is exactly one callee (namely, `pow2`) and it
// lives in the root context.
struct Context {
  Context(ir::Module *ir_mod);
  Context(Context const &) = delete;

  // Even though these special members are defaulted, they need to be defined
  // externally because otherwise we would generate the corresponding special
  // members for the incomplete type `Subcontext` below.
  Context(Context &&);
  ~Context();

  std::string DebugString() const;

  Context &root() & { return tree_.parent ? tree_.parent->root() : *this; }
  Context const &root() const & {
    return tree_.parent ? tree_.parent->root() : *this;
  }
  bool is_root() const { return tree_.parent == nullptr; }

  // Returns a Context object which has `this` as it's parent, but for which
  // `this` is not aware of the returned subcontext. This allows us to use the
  // return object as a scratchpad for computations before we know whether or
  // not we want to keep such computations. The canonical example of this is
  // when handling compile-time parameters to generic functions or structs. We
  // need to compute all parameters and arguments, but may want to throw away
  // that context if either (a) an instantiation already exists, or (b) there
  // was a substitution failure.
  //
  // TODO: Scratpads can still mutate actual output, so we should actually be
  // stashing their IR emissions elsewhere and merging if they turn out to be
  // okay.
  Context ScratchpadSubcontext();

  // InsertSubcontext:
  //
  // Returns an `InsertSubcontext`. The `inserted` bool member indicates whether
  // a dependency was inserted. In either case (inserted or already present) the
  // reference members `params` and `context` refer to the correspondingly
  // computed parameter types and `Context` into which new computed data
  // dependent on this set of generic context can be added.
  struct InsertSubcontextResult {
    BoundParameters const &params;
    std::vector<type::Type> &rets;
    Context &context;
    bool inserted;
  };

  InsertSubcontextResult InsertSubcontext(
      ast::ParameterizedExpression const *node, BoundParameters const &params,
      Context &&context);

  // FindSubcontext:
  //
  // Returns a `FindSubcontextResult`. The `context` reference member refers to
  // subcontext (child subcontext, not descendant) associated with the given set
  // of parameters. This subcontext will not be created if it does not already
  // exist. It must already exist under penalty of undefined behavior.
  struct FindSubcontextResult {
    type::Function const *fn_type;
    Context &context;
  };

  FindSubcontextResult FindSubcontext(ast::ParameterizedExpression const *node,
                                      BoundParameters const &params);

  // Returns a span over a the qualified types for this expression. The span may
  // be empty if the expression's type is nothing, but behavior is undefined if
  // the expressions type is not available. The returned span is valid
  // accessible for the lifetime of this Context.
  absl::Span<type::QualType const> qual_types(
      ast::Expression const *expr) const;

  // Same as `qual_types` defined above, except that behavior is defined to
  // return a default constructed span if the expression's type is not
  // available.
  absl::Span<type::QualType const> maybe_qual_type(
      ast::Expression const *expr) const;

  // Stores the QualTypes in this context, associating them with the given
  // expression.
  absl::Span<type::QualType const> set_qual_types(
      ast::Expression const *expr, absl::Span<type::QualType const> qts);
  absl::Span<type::QualType const> set_qual_type(ast::Expression const *expr,
                                                 type::QualType const qts);


  ir::ModuleId imported_module(ast::Import const *node);
  void set_imported_module(ast::Import const *node, ir::ModuleId module_id);

  void ForEachCompiledFn(
      std::invocable<ir::CompiledFn const *> auto &&f) const {
    for (auto const &compiled_fn : ir_module_.functions()) { f(&compiled_fn); }
  }

  void ForEachCompiledFn(
      std::invocable<ir::CompiledFn const *, module::Linkage> auto &&f) const {
    for (auto const &compiled_fn : ir_module_.functions()) {
      f(&compiled_fn, module::Linkage::Internal);
    }
  }

  // TODO Audit everything below here
  std::pair<ir::NativeFn, bool> add_func(
      ast::ParameterizedExpression const *expr) {
    type::Function const *fn_type =
        &qual_types(expr)[0].type().as<type::Function>();

    auto [iter, inserted] = ir_funcs_.try_emplace(expr);
    auto &entry           = iter->second;

    if (inserted) { entry = ir_module_.InsertFunction(fn_type); }
    return std::pair(entry, inserted);
  }

  ir::NativeFn FindNativeFn(ast::ParameterizedExpression const *expr) {
    auto iter = ir_funcs_.find(expr);
    if (iter != ir_funcs_.end()) { return iter->second; }
    if (parent()) { return parent()->FindNativeFn(expr); }
    return ir::NativeFn();
  }

  std::pair<ir::Scope, bool> add_scope(
      ast::ParameterizedExpression const *expr) {
    type::Scope const *scope_type =
        &qual_types(expr)[0].type().as<type::Scope>();

    auto [iter, inserted] = ir_scopes_.try_emplace(expr);
    auto &entry           = iter->second;

    if (inserted) { entry = ir_module_.InsertScope(scope_type); }
    return std::pair(entry, inserted);
  }

  ir::Scope FindScope(ast::ParameterizedExpression const *expr) {
    auto iter = ir_scopes_.find(expr);
    if (iter != ir_scopes_.end()) { return iter->second; }
    if (parent()) { return parent()->FindScope(expr); }
    return ir::Scope();
  }

  void CompleteType(ast::Expression const *expr, bool success);

  void LoadConstant(ast::Declaration::Id const *id,
                    ir::PartialResultBuffer &out) const;
  bool TryLoadConstant(ast::Declaration::Id const *id,
                       ir::PartialResultBuffer &out) const;

  type::Type arg_type(std::string_view name) const {
    auto iter = arg_type_.find(name);
    return iter == arg_type_.end() ? nullptr : iter->second;
  }

  void set_arg_type(std::string_view name, type::Type t) {
    arg_type_.emplace(name, t);
  }

  absl::Span<ast::Declaration::Id const *const> decls(
      ast::Identifier const *id) const;
  void set_decls(ast::Identifier const *id,
                 std::vector<ast::Declaration::Id const *> decls);

  template <typename T>
  internal_context::DefiningAstNodeType<T> const *AstLiteral(T const *p) const {
    // TODO: Store a bidirectional map. This could be made way more efficient.
    type::Type erased_type = p;
    for (auto const &[expr, t] : types_) {
      if (t == erased_type) {
        return &expr->template as<internal_context::DefiningAstNodeType<T>>();
      }
    }
    if (auto *parent_context = parent()) {
      return parent_context->AstLiteral(p);
    }
    return nullptr;
  }

  template <typename T, typename... Args>
  std::pair<type::Type, bool> EmplaceType(ast::Expression const *expr,
                                          Args &&... args) {
    if (type::Type *t = TryLoadType(expr)) { return std::pair(*t, false); }
    auto [iter, inserted] =
        types_.emplace(expr, type::Allocate<T>(std::forward<Args>(args)...));
    return std::pair(iter->second, true);
  }

  type::Type LoadType(ast::Expression const *expr) {
    return *ASSERT_NOT_NULL(TryLoadType(expr));
  }

  ir::CompleteResultBuffer const &SetConstant(
      ast::Declaration::Id const *id, ir::CompleteResultRef const &buffer);
  ir::CompleteResultBuffer const &SetConstant(
      ast::Declaration::Id const *id, ir::CompleteResultBuffer const &buffer);
  ir::CompleteResultBuffer const *Constant(
      ast::Declaration::Id const *id) const;

  void SetAllOverloads(ast::Expression const *callee, ast::OverloadSet os);
  ast::OverloadSet const *AllOverloads(ast::Expression const *callee) const;

  void SetViableOverloads(ast::Expression const *callee, ast::OverloadSet os) {
    viable_overloads_.emplace(callee, std::move(os));
  }

  void SetAdlModules(ast::Identifier const *callee,
                     absl::flat_hash_set<CompiledModule const *> modules) {
    adl_modules_.emplace(callee, std::move(modules));
  }
  absl::flat_hash_set<CompiledModule const *> const *AdlModules(
      ast::Identifier const *callee) {
    auto iter = adl_modules_.find(callee);
    if (iter == adl_modules_.end()) {
      if (parent()) { return parent()->AdlModules(callee); }
      return nullptr;
    } else {
      return &iter->second;
    }
  }

  ast::OverloadSet const &ViableOverloads(ast::Expression const *callee) const {
    auto iter = viable_overloads_.find(callee);
    if (iter == viable_overloads_.end()) {
      if (parent() == nullptr) {
        UNREACHABLE("Failed to find any overloads for ", callee->DebugString());
      }
      return parent()->ViableOverloads(callee);
    } else {
      return iter->second;
    }
  }

  ir::Module &ir() { return ir_module_; }

  void TrackJumps(ast::Node const *p) { jumps_.TrackJumps(p); }

  absl::Span<ast::ReturnStmt const *const> ReturnsTo(
      base::PtrUnion<ast::FunctionLiteral const,
                     ast::ShortFunctionLiteral const>
          node) const;
  absl::Span<ast::YieldStmt const *const> YieldsTo(
      base::PtrUnion<ast::BlockNode const, ast::ScopeNode const> node) const;

  bool ClaimVerifyBodyTask(ast::FunctionLiteral const *node) {
    return body_is_verified_.insert(node).second;
  }

 private:
  explicit Context(Context *parent);

  // Each Context is an intrusive node in a tree structure. Each Context has a
  // pointer to it's parent (accessible via `this->parent()`, and each node owns
  // it's children.
  struct Subcontext;
  struct ContextTree {
    Context *parent = nullptr;
    absl::flat_hash_map<
        ast::ParameterizedExpression const *,
        absl::node_hash_map<BoundParameters, std::unique_ptr<Subcontext>>>
        children;
  } tree_;
  constexpr Context *parent() { return tree_.parent; }
  constexpr Context const *parent() const { return tree_.parent; }

  type::Type *TryLoadType(ast::Expression const *expr) {
    auto iter = types_.find(expr);
    if (iter != types_.end()) { return &iter->second; }
    if (parent() == nullptr) { return nullptr; }
    return parent()->TryLoadType(expr);
  }

  absl::flat_hash_set<ast::FunctionLiteral const *> body_is_verified_;

  // Types of the expressions in this context.
  absl::flat_hash_map<ast::Expression const *, std::vector<type::QualType>>
      qual_types_;

  // Stores the types of argument bound to the parameter with the given name.
  absl::flat_hash_map<std::string_view, type::Type> arg_type_;

  // A map from each identifier to all possible declaration identifiers that the
  // identifier might refer to.
  absl::flat_hash_map<ast::Identifier const *,
                      std::vector<ast::Declaration::Id const *>>
      decls_;

  // Map of all constant declarations to their values within this dependent
  // context.
  absl::flat_hash_map<ast::Declaration::Id const *, ir::CompleteResultBuffer>
      constants_;

  // Colleciton of modules imported by this one.
  absl::flat_hash_map<ast::Import const *, ir::ModuleId> imported_modules_;

  // Overloads for a callable expression, including overloads that are not
  // callable based on the call-site arguments.
  absl::flat_hash_map<ast::Expression const *, ast::OverloadSet> all_overloads_;

  // Overloads for a callable expression, keeping only the ones that are viable
  // based on the call-site arguments.
  absl::flat_hash_map<ast::Expression const *, ast::OverloadSet>
      viable_overloads_;

  absl::node_hash_map<ast::ParameterizedExpression const *, ir::NativeFn>
      ir_funcs_;
  absl::node_hash_map<ast::ParameterizedExpression const *, ir::Scope>
      ir_scopes_;

  // Holds all information about generated IR.
  ir::Module &ir_module_;

  // The modules in which to look up a callee.
  absl::flat_hash_map<ast::Identifier const *,
                      absl::flat_hash_set<CompiledModule const *>>
      adl_modules_;

  // For types defined by a single literal expression, (e.g., enums, flags, and
  // structs), this map encodes that definition.
  absl::flat_hash_map<ast::Expression const *, type::Type> types_;

  // Provides a mapping from a given AST node to the collection of all nodes
  // that might jump to it. For example, a function literal will be mapped to
  // all return statements from that function.
  JumpMap jumps_;
};

// TODO: Probably deserves it's own translation unit?
type::Type TerminalType(ast::Terminal const &node);

}  // namespace compiler

#endif  // ICARUS_COMPILER_CONTEXT_H
