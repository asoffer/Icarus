#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include <memory>
#include <utility>

#include "ast/ast_fwd.h"
#include "base/ptr_span.h"
#include "compiler/constant/binding_map.h"
#include "compiler/constant/binding_tree.h"
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

  // Note: We do not handle embedded modules. If you want the (qual_)type_of an
  // expression that's in an embedded module, ask the module it is actually
  // defined in.
  type::Type const *type_of(ast::Expression const *expr) const;
  type::QualType const *qual_type_of(ast::Expression const *expr) const;

  ConstantBindingTree::Node const *root_node() const { return data_.root(); }
  ConstantBindingTree::Node *root_node() { return data_.root(); }

  // TODO We probably don't need these. There are likely better ways to expose
  // the requisite information.
  CompilationData const &root_data() const { return data_.root_value(); }
  CompilationData &root_data() { return data_.root_value(); }

 private:
  ConstantBindingMap<CompilationData> data_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
