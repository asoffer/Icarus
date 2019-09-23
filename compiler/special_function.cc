#include "compiler/special_function.h"
#include "ast/ast.h"
#include "ir/any_func.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "compiler/compiler.h"
#include "compiler/compiler.h"

namespace compiler {
std::optional<ir::AnyFunc> SpecialFunction(Compiler *compiler,
                                           type::Struct const *s,
                                           char const *symbol) {
  auto *ptr_to_s = type::Ptr(s);
  for (auto const *decl : s->scope_->AllDeclsWithId(symbol)) {
    // Note: there cannot be more than one declaration with the correct type
    // because our shadowing checks would have caught it.
    auto *t = compiler->type_of(decl);
    if (t == nullptr) { continue; }
    auto *fn_type = t->if_as<type::Function>();
    if (fn_type == nullptr) { continue; }
    if (fn_type->input.front() != ptr_to_s) { continue; }
    return decl->EmitValue(compiler).get<ir::AnyFunc>(0).value();
  }
  return std::nullopt;
}
}  // namespace compiler
