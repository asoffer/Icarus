#ifndef ICARUS_COMPILER_DATA_H
#define ICARUS_COMPILER_DATA_H

#include <forward_list>
#include <memory>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/node_hash_map.h"
#include "absl/hash/hash.h"
#include "ast/ast.h"
#include "ast/ast_fwd.h"
#include "base/guarded.h"
#include "base/lazy_convert.h"
#include "compiler/constant/binding.h"
#include "ir/block_def.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/jump.h"
#include "ir/results.h"
#include "ir/scope_def.h"
#include "ir/value/reg.h"
#include "module/module.h"
#include "module/pending.h"
#include "type/qual_type.h"

namespace compiler {
struct LibraryModule;

// DependentComputedData holds all data that the compiler computes about the
// program by traversing the syntax tree. This includes type information,
// compiled functions and jumps, etc. Note that this data may be dependent on
// constant parameters to a function, jump, or struct. To account for such
// dependencies, DependentComputedData is intrusively a tree. Each
// DependentComputedData has a pointer to it's parent (except the root whose
// parent-pointer is null), as well as a map keyed on arguments whose values
// hold child DependentComputedData.
//
// For instance, the program
// ```
// f ::= (n :: int64) -> () {
//   size ::= n * n
//   arr: [size; bool]
//   ...
// }
//
// f(1)
// f(2)
// ```
//
// would have there DependentComputedData nodes. The root node, which has the
// other two nodes as children. These nodes are keyed on the arguments to `f`,
// one where `n` is 1 and one where `n` is 2. Note that the type of `array` is
// not available at the root node as it's type is dependent on `n`. Rather, on
// the two child nodes it has type `[1; bool]` and `[4; bool]` respectively.
// Moreover, even the type of `size` (despite always being `int64` is not
// available on the root node. Instead, it is available on all child nodes with
// the same value of `int64`.
//
// TODO audit.
struct DependentComputedData {
  explicit DependentComputedData(module::BasicModule *mod);
  ~DependentComputedData();

  ir::ScopeDef *add_scope(module::BasicModule const *mod,
                          type::Type const *state_type) {
    return &scope_defs_.emplace_front(mod, state_type);
  }
  ir::BlockDef *add_block() { return &block_defs_.emplace_front(); }

  module::BasicModule *mod_;
  ir::Builder &bldr_;

  ir::Reg addr(ast::Declaration const *decl) const {
    auto iter = addr_.find(decl);
    if (iter != addr_.end()) { return iter->second; }
    if (parent_) { return parent_->addr(decl); }
    UNREACHABLE();
  }

  // Returns a pointer to the `ir::Jump` corresponding to the compilation of
  // `expr`, if it exists. Otherwise, returns a null pointer.
  ir::Jump *jump(ast::Jump const *expr);

  // If an  `ir::Jump` corresponding to the compilation of `expr` already
  // exists, returns a pointer to that object. Otherwise, constructs a new one
  // by calling `fn`.
  template <typename Fn>
  ir::Jump *add_jump(ast::Jump const *expr, Fn &&fn) {
    auto [iter, success] =
        jumps_.emplace(expr, base::lazy_convert{std::forward<Fn>(fn)});
    ASSERT(success == true);
    return &iter->second;
  }

  type::QualType const *result(ast::Expression const *expr) const;
  type::QualType set_result(ast::Expression const *expr, type::QualType r);

  // TODO this is transient compiler state and therefore shouldn't be stored in
  // `DependentComputedData`.
  // During validation, when a cyclic dependency is encountered, we write it
  // down here. That way, we can bubble up from the dependency until we see it
  // again, at each step adding the nodes to the error log involved in the
  // dependency. Once complete, we reset this to null
  std::vector<ast::Identifier const *> cyc_deps_;

  // TODO this is transient compiler state and therefore shouldn't be stored in
  // `DependentComputedData`.
  base::guarded<absl::node_hash_map<ast::Node const *, base::move_func<void()>>>
      deferred_work_;

  ir::NativeFnSet fns_;

  // std::forward_list makes sense for many of the strutures below because we
  // never traverse them and we need pointer stability. A vector of unique_ptrs
  // would also work, but would unnecessarily reallocate with some frequency.
  std::forward_list<ir::ScopeDef> scope_defs_;
  std::forward_list<ir::BlockDef> block_defs_;

  absl::flat_hash_map<ast::Expression const *, type::QualType>
      type_verification_results_;

  absl::flat_hash_map<ast::Declaration const *, ir::Reg> addr_;

  absl::flat_hash_map<ast::Import const *, module::Pending<LibraryModule>>
      imported_module_;

  absl::flat_hash_map</* to = */ ast::Node const *,
                      /* from = */ std::vector<ast::Node const *>>
      extraction_map_;

  absl::flat_hash_map<type::Type const *, ir::NativeFn> init_, copy_assign_,
      move_assign_, destroy_;

  // InsertDependent:
  //
  // Returns an `InsertDependentResult`. The `inserted` bool member  indicates
  // whether a dependency was inserted. In either case (inserted or already
  // present) the reference members `params` and `data` refer to the
  // correspondingly computed parameter types and `DependentComputedData` into
  // which new computed data dependent on this set of generic context can be
  // added.
  struct InsertDependentResult {
    core::Params<type::Type const *> &params;
    DependentComputedData &data;
    bool inserted;
  };

  InsertDependentResult InsertDependent(
      ast::ParameterizedExpression const *node,
      core::FnArgs<type::Typed<std::optional<ir::Value>>> const &args);

  // FindDependent:
  //
  // Returns a `FindDependentResult`. The reference members `params` and `data`
  // refer to the correspondingly computed parameter types and
  // `DependentComputedData` into which new computed data dependent on this set
  // of generic context can be added. Such a `DependentComputedData` must
  // already be present as a child.
  struct FindDependentResult {
    core::Params<type::Type const *> &params;
    DependentComputedData &data;
  };

  FindDependentResult FindDependent(
      ast::ParameterizedExpression const *node,
      core::FnArgs<type::Typed<std::optional<ir::Value>>> const &args);

  template <
      typename Ctor,
      std::enable_if_t<base::meta<Ctor> != base::meta<ir::NativeFn>, int> = 0>
  ir::NativeFn EmplaceNativeFn(ast::Expression const *expr, Ctor &&ctor) {
    return ir_funcs_.emplace(expr, base::lazy_convert(std::forward<Ctor>(ctor)))
        .first->second;
  }

  base::untyped_buffer_view LoadConstantParam(ast::Declaration const *decl) {
    auto buf_view = constants_.get_constant(decl);
    if (not buf_view.empty()) { return buf_view; }
    if (parent_) { buf_view = parent_->LoadConstantParam(decl); }
    return buf_view;
  }

  ir::NativeFn *FindNativeFn(ast::Expression const *expr) {
    auto iter = ir_funcs_.find(expr);
    if (iter != ir_funcs_.end()) { return &iter->second; }
    if (parent_) { return parent_->FindNativeFn(expr); }
    return nullptr;
  }

  type::Type const *arg_type(std::string_view name) const {
    auto iter = arg_type_.find(name);
    return iter == arg_type_.end() ? nullptr : iter->second;
  }

  void set_arg_type(std::string_view name, type::Type const *t) {
    arg_type_.emplace(name, t);
  }

  ir::Value arg_value(std::string_view name) const {
    auto iter = arg_val_.find(name);
    return iter == arg_val_.end() ? nullptr : iter->second;
  }

  void set_arg_value(std::string_view name, ir::Value const &value) {
    arg_val_.emplace(name, value);
  }

  ConstantBinding constants_;

  absl::flat_hash_map<ast::ReturnStmt const *, type::Function const *>
      return_to_fn_type_;

 private:
  // Stores the types of argument bound to the parameter with the given name.
  absl::flat_hash_map<std::string_view, type::Type const *> arg_type_;
  // TODO: If you could store the decl on the ast-node you could use ConstantBinding for this.
  absl::flat_hash_map<std::string_view, ir::Value> arg_val_;

  struct DependentDataChild {
    // TODO Swiss-tables do not store the hash, but recomputing the argument
    // hash on each lookup can be expensive. Instead, we can use the
    // optimization technique where we store the arguments in a separate vector,
    // and store the hash and an index into the vector. In fact, just storing
    // the hash adjacent is probably a good chunk of the wins anyway.
    struct ArgsHash {
      size_t operator()(
          core::FnArgs<type::Typed<std::optional<ir::Value>>> const &args)
          const {
        // Ew, this hash is awful. Make this better.
        std::vector<
            std::tuple<std::string_view, type::Type const *, bool, ir::Value>>
            elems;
        for (auto const &arg : args.pos()) {
          elems.emplace_back("", arg.type(), arg->has_value(),
                             arg->value_or(false));
        }
        for (auto const &[name, arg] : args.named()) {
          elems.emplace_back(name, arg.type(), arg->has_value(),
                             arg->value_or(false));
        }
        std::sort(elems.begin(), elems.end(),
                  [](auto const &lhs, auto const &rhs) {
                    return std::get<0>(lhs) < std::get<0>(rhs);
                  });
        return absl::Hash<decltype(elems)>{}(elems);
      }
    };

    DependentComputedData *parent = nullptr;
    // TODO keying on arguments is actually wrong, because specifying the same
    // arguments in a named vs positional manner should produce the same
    // function. We may still want to cache them but we should be keying on
    // parameters. But this also means we need to run the arg/param matching
    // code every time.
    absl::node_hash_map<
        core::FnArgs<type::Typed<std::optional<ir::Value>>>,
        std::pair<core::Params<type::Type const *>, DependentComputedData>,
        ArgsHash>
        map;
  };

  // The parent node containing the generic that is instantiated to produce this
  // `DependentComputedData`.
  DependentComputedData *parent_ = nullptr;
  absl::flat_hash_map<ast::ParameterizedExpression const *, DependentDataChild>
      dependent_data_;

  // All functions, whether they're directly compiled or generated by a generic.
  absl::node_hash_map<ast::Expression const *, ir::NativeFn> ir_funcs_;
  // All jumps, whether they're directly compiled or generated by a generic.
  absl::node_hash_map<ast::Jump const *, ir::Jump> jumps_;
};

}  // namespace compiler
#endif  // ICARUS_COMPILER_DATA_H
