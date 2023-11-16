#include "ir/builtin_module.h"

#include <string_view>

#include "common/identifier.h"
#include "ir/global_function_registry.h"
#include "nth/utility/no_destructor.h"
#include "type/type.h"

namespace ic {

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

nth::NoDestructor<IrFunction> Opaque([] {
  IrFunction f(0, 1);
  f.append<ConstructOpaqueType>();
  f.append<jasmin::Return>();
  return f;
}());

nth::NoDestructor<IrFunction> Arguments([] {
  IrFunction f(0, 2);
  f.append<LoadProgramArguments>();
  f.append<jasmin::Return>();
  return f;
}());

nth::NoDestructor<IrFunction> Slice([] {
  IrFunction f(2, 2);
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
  m.Insert(
      Identifier("opaque"),
      {.qualified_type = type::QualifiedType::Constant(type::Function(
           type::Parameters(std::vector<type::ParametersType::Parameter>{}),
           {type::Type_})),
       .value          = {jasmin::Value(&*Opaque)}});
  global_function_registry.Register(
      FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)), &*Opaque);

  m.Insert(
      Identifier("arguments"),
      {.qualified_type = type::QualifiedType::Constant(type::Function(
           type::Parameters(std::vector<type::ParametersType::Parameter>{}),
           {type::Slice(type::Slice(type::Char))})),
       .value          = {jasmin::Value(&*Arguments)}});
  global_function_registry.Register(
      FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)), &*Arguments);

  m.Insert(Identifier("slice"),
           {.qualified_type = type::QualifiedType::Constant(type::Function(
                type::Parameters(std::vector<type::ParametersType::Parameter>{
                    {.type = type::BufPtr(type::Char)}, {.type = type::U64}}),
                {type::Slice(type::Char)})),
            .value          = {jasmin::Value(&*Slice)}});
  global_function_registry.Register(
      FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)), &*Slice);

  m.Insert(
      Identifier("foreign"),
      {.qualified_type = type::QualifiedType::Constant(type::GenericFunction(
           type::Evaluation::RequireCompileTime, &*ForeignType)),
       .value          = {&*Foreign}});
  global_function_registry.Register(
      FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)),
      &*ForeignType);

  // TODO: There's something wrong with registration happening after this point.
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
