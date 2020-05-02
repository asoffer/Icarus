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

// TODO this struct has developed some cruft. Probably some fields are unused at
// this point. Audit.
struct CompilationData {
  explicit CompilationData(module::BasicModule *mod);
  ~CompilationData();

  ir::ScopeDef *add_scope(module::BasicModule const *mod,
                          type::Type const *state_type) {
    return &scope_defs_.emplace_front(mod, state_type);
  }
  ir::BlockDef *add_block() { return &block_defs_.emplace_front(); }

  module::BasicModule *mod_;
  ir::Builder &bldr_;

  ir::Jump *jump(ast::Jump const *expr) {
    auto iter = jumps_.find(expr);
    if (iter == jumps_.end()) { return nullptr; }
    return &iter->second;
  }

  type::QualType const *result(ast::Expression const *expr) const {
    auto iter = type_verification_results_.find(expr);
    return iter == type_verification_results_.end() ? nullptr : &iter->second;
  }

  type::QualType set_result(ast::Expression const *expr, type::QualType r) {
    type_verification_results_.emplace(expr, r);
    return r;
  }

  template <typename Fn>
  ir::Jump *add_jump(ast::Jump const *expr, Fn &&fn) {
    auto [iter, success] =
        jumps_.emplace(expr, base::lazy_convert{std::forward<Fn>(fn)});
    ASSERT(success == true);
    return &iter->second;
  }

  // TODO this is transient compiler state and therefore shouldn't be stored in
  // `CompilationData`.
  // During validation, when a cyclic dependency is encountered, we write it
  // down here. That way, we can bubble up from the dependency until we see it
  // again, at each step adding the nodes to the error log involved in the
  // dependency. Once complete, we reset this to null
  std::vector<ast::Identifier const *> cyc_deps_;

  // TODO Because you already have arguments, it's perhaps better to just be a
  // pointer into the arguments buffer, to avoid the
  // reallocation/double-storage, but we can deal with this later. Probably
  // requires a deeper refactoring to have things linke ir::ResultView, etc.
  absl::flat_hash_map<ir::Reg, ir::Results> *inline_ = nullptr;

  // TODO this is transient compiler state and therefore shouldn't be stored in
  // `CompilationData`.
  base::guarded<absl::node_hash_map<ast::Node const *, base::move_func<void()>>>
      deferred_work_;

  ir::NativeFnSet fns_;

  // std::forward_list makes sense for many of the strutures below because we
  // never traverse them and we need pointer stability. A vector of unique_ptrs
  // would also work, but would unnecessarily reallocate with some frequency.
  std::forward_list<ir::ScopeDef> scope_defs_;
  std::forward_list<ir::BlockDef> block_defs_;

  absl::node_hash_map<ast::Jump const *, ir::Jump> jumps_;

  absl::flat_hash_map<ast::Expression const *, type::QualType>
      type_verification_results_;

  absl::flat_hash_map<ast::Declaration const *, ir::Reg> addr_;

  // TODO probably make these funcs constant.
  absl::node_hash_map<ast::Expression const *, ir::NativeFn> ir_funcs_;

  absl::flat_hash_map<ast::Import const *, module::Pending<LibraryModule>>
      imported_module_;

  absl::flat_hash_map</* to = */ ast::Node const *,
                      /* from = */ std::vector<ast::Node const *>>
      extraction_map_;

  absl::flat_hash_map<type::Type const *, ir::NativeFn> init_, copy_assign_,
      move_assign_, destroy_;

  // TODO Swiss-tables do not store the hash, but recomputing the argument hash
  // on each lookup can be expensive. Instead, we can use the optimization
  // technique where we store the arguments in a separate vector, and store the
  // hash and an index into the vector. In fact, just storing the hash adjacent
  // is probably a good chunk of the wins anyway.
  struct ArgsHash {
    size_t operator()(
        core::FnArgs<type::Typed<std::optional<ir::Value>>> const &args) const {
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

  // ast::ParameterizedExpression const *parent_ = nullptr;

  ConstantBinding constants_;
  absl::flat_hash_map<
      ast::ParameterizedExpression const *,
      absl::node_hash_map<
          core::FnArgs<type::Typed<std::optional<ir::Value>>>,
          std::pair<core::Params<type::Type const *>, CompilationData>,
          ArgsHash>>
      dependent_data_;
};

}  // namespace compiler
#endif  // ICARUS_COMPILER_DATA_H
