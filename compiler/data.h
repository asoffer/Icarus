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
#include "ir/scope_def.h"
#include "ir/value/reg.h"
#include "ir/value/value.h"
#include "module/module.h"
#include "type/qual_type.h"

namespace compiler {
struct LibraryModule;
struct CompiledModule;

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
  explicit DependentComputedData(CompiledModule *mod);
  ~DependentComputedData();

  ir::ScopeDef *add_scope(CompiledModule const *mod,
                          type::Type const *state_type) {
    // TODO reinterpret_cast because of awful layering issue.
    return &scope_defs_.emplace_front(
        reinterpret_cast<module::BasicModule const *>(mod), state_type);
  }
  ir::BlockDef *add_block() { return &block_defs_.emplace_front(); }

  CompiledModule *module() const { return mod_; }

  CompiledModule *mod_;

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

  type::QualType set_qual_type(ast::Expression const *expr, type::QualType qt);
  type::QualType const *qual_type(ast::Expression const *expr) const;

  void set_addr(ast::Declaration const *decl, ir::Reg addr) {
    addr_.emplace(decl, addr);
  }


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

  absl::flat_hash_map</* to = */ ast::Node const *,
                      /* from = */ std::vector<ast::Node const *>>
      extraction_map_;

  absl::flat_hash_map<type::Type const *, ir::NativeFn> init_, copy_assign_,
      move_assign_, destroy_;

  LibraryModule *imported_module(ast::Import const *node);
  void set_imported_module(ast::Import const *node, LibraryModule *module);

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
    std::vector<type::Type const *> &rets;
    DependentComputedData &data;
    bool inserted;
  };

  InsertDependentResult InsertDependent(
      ast::ParameterizedExpression const *node,
      core::Params<type::Type const *> const &params);

  // FindDependent:
  //
  // Returns a `FindDependentResult`. The reference members `params` and `data`
  // refer to the correspondingly computed parameter types and
  // `DependentComputedData` into which new computed data dependent on this set
  // of generic context can be added. Such a `DependentComputedData` must
  // already be present as a child.
  struct FindDependentResult {
    type::Function const *fn_type;
    DependentComputedData &data;
  };

  FindDependentResult FindDependent(
      ast::ParameterizedExpression const *node,
      core::Params<type::Type const *> const &params);

  template <
      typename Ctor,
      std::enable_if_t<base::meta<Ctor> != base::meta<ir::NativeFn>, int> = 0>
  ir::NativeFn EmplaceNativeFn(ast::Expression const *expr, Ctor &&ctor) {
    return ir_funcs_.emplace(expr, base::lazy_convert(std::forward<Ctor>(ctor)))
        .first->second;
  }

  ir::Value LoadConstantParam(ast::Declaration const *decl) {
    ir::Value val = constants_.get_constant(decl);
    if (not val.empty()) { return val; }
    if (parent_) { val = parent_->LoadConstantParam(decl); }
    return val;
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
    return iter == arg_val_.end() ? ir::Value() : iter->second;
  }

  void set_arg_value(std::string_view name, ir::Value const &value) {
    arg_val_.emplace(name, value);
  }

  ConstantBinding constants_;

 private:
  // Stores the types of argument bound to the parameter with the given name.
  absl::flat_hash_map<std::string_view, type::Type const *> arg_type_;
  // TODO: If you could store the decl on the ast-node you could use ConstantBinding for this.
  absl::flat_hash_map<std::string_view, ir::Value> arg_val_;

  // Colleciton of modules imported by this one.
  absl::flat_hash_map<ast::Import const *, LibraryModule *> imported_modules_;

  struct DependentDataChild {
    DependentComputedData *parent = nullptr;
    struct DataImpl;
    absl::flat_hash_map<core::Params<type::Type const *>,
                        std::unique_ptr<DataImpl>>
        map;
  };

  // The parent node containing the generic that is instantiated to produce this
  // `DependentComputedData`.
 public:
  DependentComputedData *parent_ = nullptr;

 private:
  std::unordered_map<ast::ParameterizedExpression const *, DependentDataChild>
      dependent_data_;

  // All functions, whether they're directly compiled or generated by a generic.
  absl::node_hash_map<ast::Expression const *, ir::NativeFn> ir_funcs_;
  // All jumps, whether they're directly compiled or generated by a generic.
  absl::node_hash_map<ast::Jump const *, ir::Jump> jumps_;
};

}  // namespace compiler
#endif  // ICARUS_COMPILER_DATA_H
