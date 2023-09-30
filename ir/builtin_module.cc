#include "ir/builtin_module.h"

#include <string_view>

#include "lexer/token_buffer.h"
#include "nth/utility/no_destructor.h"

namespace ic {

nth::NoDestructor<IrFunction> Print([] {
  IrFunction f(0, 1);
  f.append<PrintHelloWorld>(true);
  f.append<jasmin::Push>(true);
  return f;
}());

Module BuiltinModule(TokenBuffer& token_buffer) {
  Module m;
  m.Insert(token_buffer.IdentifierIndex("hello_world"),
           {.qualified_type = type::QualifiedType(
                type::Qualifier::Constant(),
                type::Function(type::Parameters({}), {type::Bool})),
            .value = {jasmin::Value(&*Print)}});
  return m;
}

}  // namespace ic
