#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include <list>
#include <memory>
#include <utility>

#include "ast/ast_fwd.h"
#include "base/ptr_span.h"
#include "compiler/constant_binding.h"
#include "compiler/dependent_data.h"
#include "module/module.h"
#include "type/type_fwd.h"

namespace compiler {

struct CompiledModule : module::ExtendedModule<CompiledModule> {
  // Even though ew only ever want to pass a specific well-known ProcessFn here
  // the one that does compilation, it requires constructing a Compiler object
  // and this would create a dependency cycle. To avoid that we need to pass it
  // in as an argument.
  template <typename ProcessFn,
            typename std::enable_if_t<
                not std::is_same_v<ProcessFn, CompiledModule>, int> = 0>
  explicit CompiledModule(ProcessFn fn)
      : module::ExtendedModule<CompiledModule>(std::move(fn)) {}

  type::Type const *type_of(ast::Expression const *expr) const;


  // TODO make private

  // TODO It's possible to have layers of constant bindings in a tree-like
  // structure. For example,
  //   f :: (a :: int64) => (b :: int64) => (c :: int64) => a + b * c
  // has 3 layers. Essentially the number of layers is the number of nested
  // scopes that have constant parameters (at time of writing only functions and
  // struct literals, though struct literals may not be specified as constants
  // syntactically?). For now you just store them flat in this vector and check
  // them potentially many times. Perhaps a tree-like structure would be more
  // efficient? More cache misses, but you're already paying heavily for the
  // equality call, so maybe it's just a simpler structure.
  //
  // std::list makes sense here because we never traverse them and we need
  // pointer stability. A vector of unique_ptrs would also work, but would
  // unnecessarily reallocate with some frequency..
  std::list<std::pair<ConstantBinding, DependentData>> dep_data_;

  std::vector<std::unique_ptr<ir::CompiledFn>> fns_;
  std::vector<std::unique_ptr<ir::ScopeDef>> scope_defs_;
  std::vector<std::unique_ptr<ir::BlockDef>> block_defs_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
