#include "compiler/constant/binding.h"
#include "ast/ast.h"
#include "type/type.h"

namespace compiler {

base::untyped_buffer_view ConstantBinding::get_constant(
    ast::Declaration const* decl) const {
  auto iter = bindings_.find(decl);
  if (iter == bindings_.end()) { return base::untyped_buffer_view{}; }
  return iter->second.buf_;
}

base::untyped_buffer_view ConstantBinding::reserve_slot(
    ast::Declaration const* decl, type::Type const* t) {
  auto [iter, newly_inserted] =
      bindings_.emplace(decl, Binding{t, base::untyped_buffer{}});
  return iter->second.buf_;
}

void ConstantBinding::set_slot(ast::Declaration const* decl,
                               base::untyped_buffer_view buf) {
  auto& to_buf = bindings_[decl].buf_;
  ASSERT(to_buf.size() == 0u);
  to_buf.append_bytes(buf.size());
  std::memcpy(to_buf.raw(0), buf.raw(0), buf.size());
}

}  // namespace compiler
