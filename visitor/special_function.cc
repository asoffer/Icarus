#include "visitor/special_function.h"
#include "ast/ast.h"
#include "ir/any_func.h"
#include "misc/context.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "visitor/emit_ir.h"
#include "visitor/traditional_compilation.h"

namespace visitor {
std::optional<ir::AnyFunc> SpecialFunction(TraditionalCompilation *visitor,
                                           type::Struct const *s,
                                           char const *symbol) {
  auto *ptr_to_s = type::Ptr(s);
  for (auto const *decl : s->scope_->AllDeclsWithId(symbol)) {
    // Note: there cannot be more than one declaration with the correct type
    // because our shadowing checks would have caught it.
    auto *t = visitor->context().type_of(decl);
    if (t == nullptr) { continue; }
    auto *fn_type = t->if_as<type::Function>();
    if (fn_type == nullptr) { continue; }
    if (fn_type->input.front() != ptr_to_s) { continue; }
    return decl->EmitValue(visitor).get<ir::AnyFunc>(0).val_;
  }
  return std::nullopt;
}
}  // namespace visitor
