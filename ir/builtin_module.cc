#include "ir/builtin_module.h"

#include <string_view>

#include "common/identifier.h"
#include "ir/global_function_registry.h"
#include "nth/utility/no_destructor.h"
#include "type/type.h"

namespace ic {
namespace {
nth::NoDestructor<IrFunction> Function([] {
  IrFunction f(1, 1);
  f.append<TypeKind>();
  f.append<jasmin::Push>(type::Type::Kind::Function);
  f.append<jasmin::Equal<type::Type::Kind>>();
  f.append<jasmin::Return>();
  return f;
}());

nth::NoDestructor<IrFunction> AsciiEncodeFn([] {
  IrFunction f(1, 1);
  f.append<AsciiEncode>();
  f.append<jasmin::Return>();
  return f;
}());

nth::NoDestructor<IrFunction> AsciiDecodeFn([] {
  IrFunction f(1, 1);
  f.append<AsciiDecode>();
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

nth::NoDestructor<std::vector<std::string>> BuiltinNamesImpl;

}  // namespace

std::span<std::string const> BuiltinNames() { return *BuiltinNamesImpl; }

Module BuiltinModule() {
  uint32_t next_id = 0;
  Module m;

  auto Register = [&](std::string_view name, type::Type t,
                      IrFunction const& f) {
    m.Insert(Identifier(name),
             {.qualified_type = type::QualifiedType::Constant(t),
              .value          = {jasmin::Value(&f)}});
    global_function_registry.Register(
        FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)), &f);
    BuiltinNamesImpl->emplace_back(name);
  };

  Register("opaque",
           type::Function(
               type::Parameters(std::vector<type::ParametersType::Parameter>{}),
               {type::Type_}),
           *Opaque);

  Register("arguments",
           type::Function(
               type::Parameters(std::vector<type::ParametersType::Parameter>{}),
               {type::Slice(type::Slice(type::Char))}),
           *Arguments);

  Register("ascii_encode",
           type::Function(
               type::Parameters(std::vector<type::ParametersType::Parameter>{
                   {.type = type::U8}}),
               {type::Char}),
           *AsciiEncodeFn);

  Register("ascii_decode",
           type::Function(
               type::Parameters(std::vector<type::ParametersType::Parameter>{
                   {.type = type::Char}}),
               {type::U8}),
           *AsciiDecodeFn);

  Register("slice",
           type::Function(
               type::Parameters(std::vector<type::ParametersType::Parameter>{
                   {.type = type::BufPtr(type::Char)}, {.type = type::U64}}),
               {type::Slice(type::Char)}),
           *Slice);

  // TODO: There's something wrong with registration happening after this point.
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

  return m;
}

}  // namespace ic
