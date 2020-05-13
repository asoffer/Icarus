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
  return bindings_.emplace(decl, Binding{ASSERT_NOT_NULL(t), ir::Value()})
      .first->second.value;
}

void ConstantBinding::set_slot(ast::Declaration const* decl,
                               ir::Value const& val) {
  auto iter = bindings_.find(decl);
  ASSERT(iter != bindings_.end());
  iter->second.value = val;
}

}  // namespace compiler
