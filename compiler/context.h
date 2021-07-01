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
#include "compiler/instructions.h"
#include "compiler/jump_map.h"
#include "ir/builder.h"
#include "ir/compiled_block.h"
#include "ir/compiled_fn.h"
#include "ir/compiled_jump.h"
#include "ir/compiled_scope.h"
#include "ir/value/block.h"
#include "ir/value/reg.h"
#include "ir/value/scope.h"
#include "ir/value/value.h"
#include "module/module.h"
#include "type/qual_type.h"

namespace compiler {
struct LibraryModule;
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
  explicit Context(CompiledModule *mod);
  Context(Context const &) = delete;

  // Even though these special members are defaulted, they need to be defined
  // externally because otherwise we would generate the corresponding special
  // members for the incomplete type `Subcontext` below.
  Context(Context &&);
  ~Context();

  std::string DebugString() const;

  CompiledModule &module() const { return mod_; }

  Context &root() & { return tree_.parent ? tree_.parent->root() : *this; }
  Context const &root() const & {
    return tree_.parent ? tree_.parent->root() : *this;
  }

  // Returns a Context object which has `this` as it's parent, but for which
  // `this` is not aware of the returned subcontext. This allows us to use the
  // return object as a scratchpad for computations before we know whether or
  // not we want to keep such computations. The canonical example of this is
  // when handling compile-time parameters to generic functions or structs. We
  // need to compute all parameters and arguments, but may want to throw away
  // that context if either (a) an instantiation already exists, or (b) there
  // was a substitution failure.
  Context ScratchpadSubcontext();

  // InsertSubcontext:
  //
  // Returns an `InsertSubcontext`. The `inserted` bool member indicates whether
  // a dependency was inserted. In either case (inserted or already present) the
  // reference members `params` and `data` refer to the correspondingly computed
  // parameter types and `Context` into which new computed data dependent on
  // this set of generic context can be added.
  struct InsertSubcontextResult {
    core::Params<std::pair<ir::Value, type::QualType>> const &params;
    std::vector<type::Type> &rets;
    Context &context;
    bool inserted;
  };

  InsertSubcontextResult InsertSubcontext(
      ast::ParameterizedExpression const *node,
      core::Params<std::pair<ir::Value, type::QualType>> const &params,
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

  FindSubcontextResult FindSubcontext(
      ast::ParameterizedExpression const *node,
      core::Params<std::pair<ir::Value, type::QualType>> const &params);

  // Returns a span over a the qualified types for this expression. The span may
  // be empty if the expression's type is nothing, but behavior is undefined if
  // the expressions type has not yet been computed. The returned span is valid
  // accessible for the lifetime of this Context.
  absl::Span<type::QualType const> qual_types(
      ast::Expression const *expr) const;

  // Same as `qual_type` defined above, except that behavior is defined to
  // return a default constructed span ifthe expression's type has not yet been
  // computed.
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

  ir::Scope add_scope(type::Type state_type) {
    return ir::Scope(&scopes_.emplace_front(state_type));
  }
  ir::Block add_block() { return ir::Block(&blocks_.emplace_front()); }

  void ForEachCompiledFn(
      std::invocable<ir::CompiledFn const *> auto &&f) const {
    for (auto const &compiled_fn : fns_) { f(compiled_fn.get()); }
  }

  void ForEachCompiledFn(
      std::invocable<ir::CompiledFn const *, module::Linkage> auto &&f) const {
    for (auto const &compiled_fn : fns_) {
      f(compiled_fn.get(), module::Linkage::Internal);
    }
  }


  // TODO Audit everything below here
  std::pair<ir::NativeFn, bool> add_func(
      ast::ParameterizedExpression const *expr) {
    type::Function const *fn_type =
        &qual_types(expr)[0].type().as<type::Function>();

    auto [iter, inserted] = ir_funcs_.try_emplace(expr);
    auto &entry           = iter->second;

    if (inserted) {
      auto *data = InsertFunction(
          fn_type,
          expr->params().Transform([fn_type, i = 0](auto const &d) mutable {
            return type::Typed<ast::Declaration const *>(
                d.get(), fn_type->params()[i++].value.type());
          }));

      entry = ir::NativeFn(data);
    }
    return std::pair(entry, inserted);
  }

  // If an `ir::CompiledJump` corresponding to the compilation of `expr` already
  // exists, returns a pointer to that object. Otherwise, constructs a new one
  // by calling `fn`.
  std::pair<ir::Jump, bool> add_jump(ast::Jump const *expr) {
    type::Jump const *jump_type = &qual_types(expr)[0].type().as<type::Jump>();
    auto [iter, inserted] = ir_jumps_.try_emplace(
        expr, jump_type,
        expr->params().Transform([jump_type, i = 0](auto const &decl) mutable {
          return type::Typed<ast::Declaration const *>(
              decl.get(), jump_type->params()[i++].value);
        }));
    return std::pair<ir::Jump, bool>(ir::Jump(&iter->second), inserted);
  }

  // Returns a pointer to the `ir::CompiledJump` corresponding to the
  // compilation of `expr`, if it exists. Otherwise, returns a null pointer.
  ir::CompiledJump *jump(ast::Jump const *expr);

  void CompleteType(ast::Expression const *expr, bool success);

  ir::Value LoadConstant(ast::Declaration::Id const *id) const {
    if (auto iter = constants_.find(id); iter != constants_.end()) {
      ir::Value val = iter->second.value();
      if (not val.empty()) { return val; }
    }
    if (parent()) { return parent()->LoadConstant(id); }
    return ir::Value();
  }

  ir::Value LoadConstantParam(ast::Declaration::Id const *id) const {
    return LoadConstant(id);
  }

  ir::NativeFn FindNativeFn(ast::ParameterizedExpression const *expr) {
    auto iter = ir_funcs_.find(expr);
    if (iter != ir_funcs_.end()) { return iter->second; }
    if (parent()) { return parent()->FindNativeFn(expr); }
    return ir::NativeFn();
  }

  type::Type arg_type(std::string_view name) const {
    auto iter = arg_type_.find(name);
    return iter == arg_type_.end() ? nullptr : iter->second;
  }

  void set_arg_type(std::string_view name, type::Type t) {
    arg_type_.emplace(name, t);
  }

  ir::Value arg_value(std::string_view name) const {
    auto iter = arg_val_.find(name);
    return iter == arg_val_.end() ? ir::Value() : iter->second;
  }

  void set_arg_value(std::string_view name, ir::Value const &value) {
    arg_val_.emplace(name, value);
  }

  // TODO: Use Declaration::Id instead
  absl::Span<ast::Declaration const *const> decls(
      ast::Identifier const *id) const;
  void set_decls(ast::Identifier const *id,
                 std::vector<ast::Declaration const *> decls);

  type::Struct *get_struct(ast::StructLiteral const *s) const;
  void set_struct(ast::StructLiteral const *sl, type::Struct *s);
  type::Struct *get_struct(ast::ParameterizedStructLiteral const *s) const;
  void set_struct(ast::ParameterizedStructLiteral const *sl, type::Struct *s);
  ast::Expression const *ast_struct(type::Struct const *s) const;

  bool ShouldVerifyBody(ast::Node const *node);
  void ClearVerifyBody(ast::Node const *node);

  struct ConstantValue {
    explicit ConstantValue(base::untyped_buffer buffer, bool complete)
        : complete(complete), is_big(true), buffer_(std::move(buffer)) {}
    explicit ConstantValue(ir::Value const &v, bool complete)
        : complete(complete), is_big(false) {
      buffer_.append(v);
    }

    // TODO: This is essentially a small-buffer optimization for Value-sized
    // things. Maybe we should do that explicitly.
    ir::Value value() const {
      // TODO: Do we need to track constness of addresses in the type system
      // too?
      return is_big ? ir::Value(const_cast<std::byte *>(buffer_.raw(0)))
                    : ir::Value(buffer_.get<ir::Value>(0));
    }

    base::untyped_buffer const &buffer() const { return buffer_; }

    // Whether or not the held value is complete. This may be a struct or
    // function whose body has not been emit yet.
    bool complete;
    bool is_big;

   private:
    base::untyped_buffer buffer_;
  };
  void CompleteConstant(ast::Declaration::Id const *id);
  ir::Value SetConstant(ast::Declaration::Id const *id, ir::Value const &value,
                        bool complete = false);
  ir::Value SetConstant(ast::Declaration::Id const *id,
                        base::untyped_buffer buffer, bool complete = false);

  ConstantValue const *Constant(ast::Declaration::Id const *id) const;

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

  std::pair<ir::NativeFn, bool> InsertInit(type::Type t);
  std::pair<ir::NativeFn, bool> InsertDestroy(type::Type t);
  std::pair<ir::NativeFn, bool> InsertCopyAssign(type::Type to,
                                                 type::Type from);
  std::pair<ir::NativeFn, bool> InsertMoveAssign(type::Type to,
                                                 type::Type from);
  std::pair<ir::NativeFn, bool> InsertCopyInit(type::Type to, type::Type from);
  std::pair<ir::NativeFn, bool> InsertMoveInit(type::Type to, type::Type from);

  void WriteByteCode(ir::NativeFn f) {
    auto byte_code            = EmitByteCode(*f);
    fn_data_.at(f)->byte_code = byte_code.begin();
    byte_code_.push_back(std::move(byte_code));
  }

  void TrackJumps(ast::Node const *p) { jumps_.TrackJumps(p); }

  absl::Span<ast::ReturnStmt const *const> ReturnsTo(
      base::PtrUnion<ast::FunctionLiteral const,
                     ast::ShortFunctionLiteral const>
          node) const;
  absl::Span<ast::YieldStmt const *const> YieldsTo(
      base::PtrUnion<ast::BlockNode const, ast::ScopeNode const> node) const;

 private:
  explicit Context(CompiledModule *mod, Context *parent);

  ir::NativeFn::Data const *InsertFunction(
      type::Function const *fn_type,
      core::Params<type::Typed<ast::Declaration const *>> params);

  CompiledModule &mod_;

  // Each Context is an intrusive node in a tree structure. Each Context has a
  // pointer to it's parent (accessible via `this->parent()`, and each node owns
  // it's children.
  struct Subcontext;
  struct ContextTree {
    Context *parent = nullptr;
    absl::flat_hash_map<
        ast::ParameterizedExpression const *,
        absl::node_hash_map<core::Params<std::pair<ir::Value, type::QualType>>,
                            std::unique_ptr<Subcontext>>>
        children;
  } tree_;
  constexpr Context *parent() { return tree_.parent; }
  constexpr Context const *parent() const { return tree_.parent; }

  // Types of the expressions in this context.
  absl::flat_hash_map<ast::Expression const *, std::vector<type::QualType>>
      qual_types_;

  // Stores the types of argument bound to the parameter with the given name.
  absl::flat_hash_map<std::string_view, type::Type> arg_type_;
  absl::flat_hash_map<std::string_view, ir::Value> arg_val_;

  // A map from each identifier to all possible declarations that the identifier
  // might refer to.
  absl::flat_hash_map<ast::Identifier const *,
                      std::vector<ast::Declaration const *>>
      decls_;

  // Map of all constant declarations to their values within this dependent
  // context.
  absl::flat_hash_map<ast::Declaration::Id const *, ConstantValue> constants_;

  absl::flat_hash_map<ast::StructLiteral const *, type::Struct *> structs_;
  absl::flat_hash_map<ast::ParameterizedStructLiteral const *, type::Struct *>
      param_structs_;
  absl::flat_hash_map<type::Struct *, ast::Expression const *> reverse_structs_;

  // Colleciton of modules imported by this one.
  absl::flat_hash_map<ast::Import const *, ir::ModuleId> imported_modules_;

  // TODO: I'm not sure anymore if this is necessary/what we want.
  absl::flat_hash_set<ast::Node const *> body_verification_complete_;

  // Overloads for a callable expression, including overloads that are not
  // callable based on the call-site arguments.
  absl::flat_hash_map<ast::Expression const *, ast::OverloadSet> all_overloads_;

  // Overloads for a callable expression, keeping only the ones that are viable
  // based on the call-site arguments.
  absl::flat_hash_map<ast::Expression const *, ast::OverloadSet>
      viable_overloads_;

  // All functions, whether they're directly compiled or generated by a generic.
  std::vector<std::unique_ptr<ir::CompiledFn>> fns_;
  absl::flat_hash_map<ir::NativeFn, std::unique_ptr<ir::NativeFn::Data>>
      fn_data_;
  absl::node_hash_map<ast::ParameterizedExpression const *, ir::NativeFn>
      ir_funcs_;

  // Holds the byte code for each function defined in this context. The buffer
  // itself, once inserted into the map, should never be modified. Iterators may
  // reference into the buffer. As this map is further modified, the mapped
  // values may be moved, but iterators will remain valid.
  std::vector<base::untyped_buffer> byte_code_;

  // All jumps, whether they're directly compiled or generated by a generic.
  absl::node_hash_map<ast::Jump const *, ir::CompiledJump> ir_jumps_;

  // The modules in which to look up a callee.
  absl::flat_hash_map<ast::Identifier const *,
                      absl::flat_hash_set<CompiledModule const *>>
      adl_modules_;

  // This forward_list is never iterated over, we only require pointer
  // stability.
  std::forward_list<ir::CompiledBlock> blocks_;
  std::forward_list<ir::CompiledScope> scopes_;

  absl::flat_hash_map<type::Type, ir::NativeFn> init_, destroy_;
  absl::flat_hash_map<std::pair<type::Type, type::Type>, ir::NativeFn>
      copy_assign_, move_assign_, copy_init_, move_init_;

  // Provides a mapping from a given AST node to the collection of all nodes
  // that might jump to it. For example, a function literal will be mapped to
  // all return statements from that function.
  JumpMap jumps_;
};

ir::Value ToValue(base::untyped_buffer_view buffer, type::Type t);
void FromValue(ir::Value const &v, type::Type t, base::untyped_buffer &out);

}  // namespace compiler
#endif  // ICARUS_COMPILER_CONTEXT_H
