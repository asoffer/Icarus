#include "compiler/special_function.h"

#include "ast/ast.h"
#include "ir/value/fn.h"
#include "type/function.h"
#include "type/pointer.h"

namespace compiler {
std::optional<ir::Fn> SpecialFunction(Compiler *compiler, type::Struct const *s,
                                      char const *symbol) {
  auto *ptr_to_s = type::Ptr(s);
  // NOT_YET();
  // for (auto const *decl : module::AllDeclsTowardsRoot(s->scope_, symbol)) {
  //   // Note: there cannot be more than one declaration with the correct type
  //   // because our shadowing checks would have caught it.
  //   ASSIGN_OR(continue, auto const &t, compiler->type_of(decl));
  //   ASSIGN_OR(continue, auto const &fn_type, t.if_as<type::Function>());
  //   if (fn_type.params()[0].value.type() != ptr_to_s) { continue; }
  //   return compiler->EmitValue(decl).get<ir::RegOr<ir::Fn>>().value();
  // }
  return std::nullopt;
}
}  // namespace compiler
