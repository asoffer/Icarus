#include "compiler/module.h"

#include "ast/ast.h"

namespace compiler {

type::Type const *CompiledModule::type_of(ast::Expression const *expr) const {
  auto const *result = dep_data_.front().second.result(expr);
  if (result and result->type()) { return result->type(); }

  // TODO reenabel once modules are all in core.
  // // When searching in embedded modules we intentionally look with no bound
  // // constants. Across module boundaries, a declaration can't be present
  // anyway. for (module::BasicModule const *mod :
  // mod_->scope_.embedded_modules_) {
  //   // TODO use right constants
  //   if (auto iter = mod->dep_data_.front().second.verify_results_.find(expr);
  //       iter != mod->dep_data_.front().second.verify_results_.end()) {
  //     return iter->second.type();
  //   }
  // }
  return nullptr;
}

}  // namespace compiler
