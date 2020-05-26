#include "compiler/constant/binding.h"
#include "ast/ast.h"
#include "base/debug.h"
#include "type/type.h"

namespace compiler {

ir::Value ConstantBinding::get_constant(ast::Declaration const* decl) const {
  auto iter = bindings_.find(decl);
  if (iter == bindings_.end()) { return ir::Value(); }
  return iter->second.value;
}

ir::Value ConstantBinding::reserve_slot(ast::Declaration const* decl,
                                        type::Type const* t) {
  return bindings_
      .emplace(decl, Binding{.type     = ASSERT_NOT_NULL(t),
                             .value    = ir::Value(),
                             .complete = false})
      .first->second.value;
}

void ConstantBinding::set_slot(ast::Declaration const* decl,
                               ir::Value const& val, bool complete) {
  auto iter = bindings_.find(decl);
  ASSERT(iter != bindings_.end());
  iter->second.value = val;
  iter->second.complete = complete;
}

}  // namespace compiler
