#ifndef ICARUS_COMPILER_DATA_H
#define ICARUS_COMPILER_DATA_H

#include <forward_list>
#include <memory>
#include <utility>
#include <vector>

#include "ast/ast.h"
#include "compiler/constant_binding.h"
#include "compiler/dependent_data.h"
#include "error/log.h"
#include "ir/block_def.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/reg.h"
#include "ir/results.h"
#include "ir/scope_def.h"
#include "module/module.h"

namespace compiler {
struct CompilationData {
  explicit CompilationData(module::BasicModule *mod);
  ~CompilationData();

  ir::ScopeDef *add_scope(module::BasicModule const *mod) {
    return &scope_defs_.emplace_front(mod);
  }
  ir::BlockDef *add_block() { return &block_defs_.emplace_front(); }

  module::BasicModule *mod_;
  ir::Builder &bldr_;

  std::pair<ConstantBinding, DependentData> *constants_;
  // We only want to generate at most one node for each set of constants in a
  // function literal, but we can't generate them all at once because, for
  // example:
  //   (val :: T, T :: type) -> () { ... }
  // So we need to be able to build them even when there are dependencies
  // between them. To do this, we bulid them here and then move them into the
  // module constants when they're ready.
  ConstantBinding current_constants_;

  // TODO this looks useful in bindings too. maybe give it a better name and
  // use it more frequently?
  struct YieldResult {
    YieldResult(ast::Expression const *expr, ir::Results val)
        : expr_(expr), val_(std::move(val)) {}

    ast::Expression const *expr_;
    ir::Results val_;
  };
  std::vector<std::vector<YieldResult>> yields_stack_;

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

  base::guarded<absl::node_hash_map<ast::Node const *, base::move_func<void()>>>
      deferred_work_;

  std::vector<std::unique_ptr<ir::CompiledFn>> fns_;

  // std::forward_list makes sense for many of the strutures below because we
  // never traverse them and we need pointer stability. A vector of unique_ptrs
  // would also work, but would unnecessarily reallocate with some frequency.
  std::forward_list<ir::ScopeDef> scope_defs_;
  std::forward_list<ir::BlockDef> block_defs_;

  // TODO It's possible to have layers of constant bindings in a tree-like
  // structure. For example,
  //   f :: (a :: int64) => (b :: int64) => (c :: int64) => a + b * c
  // has 3 layers. Essentially the number of layers is the number of nested
  // scopes that have constant parameters (at time of writing only functions
  // and struct literals, though struct literals may not be specified as
  // constants syntactically?). For now you just store them flat in this
  // vector and check them potentially many times. Perhaps a tree-like
  // structure would be more efficient? More cache misses, but you're already
  // paying heavily for the equality call, so maybe it's just a simpler
  // structure.
  std::forward_list<std::pair<ConstantBinding, DependentData>> dep_data_;

  error::Log error_log_;
};
}  // namespace compiler
#endif  // ICARUS_COMPILER_DATA_H
