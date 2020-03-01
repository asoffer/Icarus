#include "ir/compiled_fn.h"

#include "ast/ast.h"
#include "core/arch.h"
#include "type/function.h"

namespace ir {

CompiledFn::CompiledFn(type::Function const *fn_type,
                       core::Params<type::Typed<ast::Declaration const *>> p)
    : internal::BlockGroup(std::move(p)), type_(fn_type) {
  // TODO is this still true with variadics?
  ASSERT(params().size() == fn_type->params().size());
}

std::ostream &operator<<(std::ostream &os, ir::CompiledFn const &f) {
  return os << "\n"
            << &f << ": " << f.type_->to_string()
            << static_cast<internal::BlockGroup const &>(f);
}

}  // namespace ir
