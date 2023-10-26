#include "ir/builtin_module.h"

#include <string_view>

#include "common/identifier.h"
#include "ir/global_function_registry.h"
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

nth::NoDestructor<IrFunction> Foreign([] {
  IrFunction f(3, 1);
  f.append<RegisterForeignFunction>();
  f.append<jasmin::Return>();
  return f;
}());

nth::NoDestructor<IrFunction> ForeignType([] {
  IrFunction f(3, 1);
  f.append<jasmin::Swap>();
  f.append<jasmin::Drop>(1);
  f.append<jasmin::Swap>();
  f.append<jasmin::Drop>(1);
  f.append<jasmin::Return>();
  return f;
}());

Module BuiltinModule() {
  uint32_t next_id = 0;

  Module m;
  m.Insert(Identifier("print"),
           {.qualified_type = type::QualifiedType::Constant(type::Function(
                type::Parameters(std::vector<type::ParametersType::Parameter>{
                    {.name = Identifier("").value(), .type = type::Slice(type::Char)},
                }),
                {type::Bool})),
            .value          = {jasmin::Value(&*PrintFn)}});
  global_function_registry.Register(
      FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)), &*PrintFn);

  m.Insert(
      Identifier("foreign"),
      {.qualified_type = type::QualifiedType::Constant(type::GenericFunction(
           type::Evaluation::RequireCompileTime, &*ForeignType)),
       .value          = {&*Foreign}});
  global_function_registry.Register(
      FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)),
      &*ForeignType);
  global_function_registry.Register(
      FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)), &*Foreign);

  m.Insert(Identifier("function"),
           {.qualified_type =
                type::QualifiedType::Constant(type::Pattern(type::Type_)),
            .value = {jasmin::Value(&*Function)}});
  global_function_registry.Register(
      FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)), &*Function);

  return m;
}

}  // namespace ic
