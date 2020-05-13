#include "compiler/special_function.h"

#include "ast/ast.h"
#include "ir/value/fn.h"
#include "type/function.h"
#include "type/pointer.h"

namespace compiler {
std::optional<ir::Fn> SpecialFunction(Compiler *compiler, type::Struct const *s,
                                      char const *symbol) {
  auto *ptr_to_s = type::Ptr(s);
  for (auto const *decl : module::AllDeclsTowardsRoot(s->scope_, symbol)) {
    // Note: there cannot be more than one declaration with the correct type
    // because our shadowing checks would have caught it.
    auto *t = compiler->type_of(decl);
    if (t == nullptr) { continue; }
    auto *fn_type = t->if_as<type::Function>();
    if (fn_type == nullptr) { continue; }
    if (fn_type->params().at(0).value != ptr_to_s) { continue; }
    return compiler->EmitValue(decl).get<ir::RegOr<ir::Fn>>().value();
  }
  return std::nullopt;
}
}  // namespace compiler
