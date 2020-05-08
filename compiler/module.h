#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include <memory>
#include <utility>

#include "ast/ast_fwd.h"
#include "base/ptr_span.h"
#include "compiler/data.h"
#include "ir/compiled_fn.h"
#include "ir/jump.h"
#include "module/module.h"
#include "type/type_fwd.h"

namespace compiler {

struct CompiledModule : module::BasicModule {
  // Even though we only ever want to pass a specific well-known ProcessFn here
  // the one that does compilation, it requires constructing a Compiler object
  // and this would create a dependency cycle. To avoid that we need to pass it
  // in as an argument.
  explicit CompiledModule() : data_(this) {}
  ~CompiledModule() override {}

  // TODO We probably don't need these. There are likely better ways to expose
  // the requisite information.
  DependentComputedData const &data() const { return data_; }
  DependentComputedData &data() { return data_; }

 private:
  DependentComputedData data_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
