#include "ir/compiled_fn.h"

#include "ast/ast.h"
#include "core/arch.h"
#include "type/function.h"

namespace ir {

CompiledFn::CompiledFn(type::Function const *fn_type,
                       core::FnParams<type::Typed<ast::Declaration const *>> p)
    : internal::BlockGroup(std::move(p)), type_(fn_type) {
  // TODO is this still true with variadics?
  ASSERT(params().size() == fn_type->input.size());
}

std::ostream &operator<<(std::ostream &os, ir::CompiledFn const &f) {
  return os << "\n"
            << f.name() << ": " << f.type_->to_string()
            << static_cast<internal::BlockGroup const &>(f);
}

std::string CompiledFn::name() const {
  std::stringstream ss;
  ss << this;
  return ss.str();
}

}  // namespace ir
