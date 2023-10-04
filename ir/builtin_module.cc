#include "ir/builtin_module.h"

#include <string_view>

#include "common/resources.h"
#include "nth/utility/no_destructor.h"

namespace ic {

nth::NoDestructor<IrFunction> Print([] {
  IrFunction f(0, 1);
  f.append<PrintHelloWorld>();
  f.append<jasmin::Push>(true);
  f.append<jasmin::Return>();
  return f;
}());

Module BuiltinModule(GlobalFunctionRegistry& registry) {
  uint32_t next_id = 0;

  Module m(registry);
  m.Insert(resources.IdentifierIndex("hello_world"),
           {.qualified_type = type::QualifiedType(
                type::Qualifier::Constant(),
                type::Function(type::Parameters({}), {type::Bool})),
            .value = {jasmin::Value(&*Print)}});
  registry.Register(FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)),
                    &*Print);
  return m;
}

}  // namespace ic
