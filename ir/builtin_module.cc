#include "ir/builtin_module.h"

#include <string_view>

#include "common/resources.h"
#include "nth/utility/no_destructor.h"
#include "type/type.h"

namespace ic {

nth::NoDestructor<IrFunction> PrintFn([] {
  IrFunction f(2, 1);
  f.append<Print>();
  f.append<jasmin::Push>(true);
  f.append<jasmin::Return>();
  return f;
}());

nth::NoDestructor<IrFunction> Function([] {
  IrFunction f(1, 1);
  f.append<TypeKind>();
  f.append<jasmin::Push>(type::Type::Kind::Function);
  f.append<jasmin::Equal<type::Type::Kind>>();
  f.append<jasmin::Return>();
  return f;
}());

Module BuiltinModule(GlobalFunctionRegistry& registry) {
  uint32_t next_id = 0;

  Module m(registry);
  m.Insert(
      resources.IdentifierIndex("print"),
      {.qualified_type = type::QualifiedType(
           type::Qualifier::Constant(),
           type::Function(
               type::Parameters(std::vector<type::ParametersType::Parameter>{
                   type::ParametersType::Parameter{
                       .name = resources.IdentifierIndex(""),
                       .type = type::Slice(type::Char)},
               }),
               {type::Bool})),
       .value = {jasmin::Value(&*PrintFn)}});
  registry.Register(FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)),
                    &*PrintFn);

  m.Insert(resources.IdentifierIndex("function"),
           {.qualified_type = type::QualifiedType(type::Qualifier::Constant(),
                                                  type::Pattern(type::Type_)),
            .value          = {jasmin::Value(&*Function)}});
  registry.Register(FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)),
                    &*Function);

  return m;
}

}  // namespace ic
