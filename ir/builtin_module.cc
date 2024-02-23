#include "ir/builtin_module.h"

#include <string_view>

#include "common/any_value.h"
#include "common/identifier.h"
#include "common/pattern.h"
#include "ir/global_function_registry.h"
#include "nth/utility/no_destructor.h"
#include "type/type.h"

namespace ic {
namespace {

jasmin::ProgramFragment<InstructionSet> builtin_function_fragment;

IrFunction const &FunctionOrPointer() {
  static nth::NoDestructor<IrFunction const *> f([]() -> IrFunction const * {
    auto &f = builtin_function_fragment.declare("~function_or_pointer", 1, 1)
                  .function;
    f.append<TypeKind>();
    f.append<jasmin::Duplicate>();
    f.append<jasmin::Push<type::Type::Kind>>(type::Type::Kind::Function);
    f.append<jasmin::Equal<type::Type::Kind>>();
    f.append<jasmin::Duplicate>();
    // TODO: You should be able to appned JumpIf.
    nth::interval<jasmin::InstructionIndex> jump =
        f.append_with_placeholders<jasmin::JumpIf>();
    f.set_value(jump, 0, 6);
    f.append<jasmin::Drop>();
    f.append<jasmin::Push<type::Type::Kind>>(type::Type::Kind::Pointer);
    f.append<jasmin::Equal<type::Type::Kind>>();
    f.append<jasmin::Return>();
    return &f;
  }());
  return **f;
}

IrFunction const &AsciiEncodeFn() {
  static nth::NoDestructor<IrFunction const *> f([]() -> IrFunction const * {
    auto &f = builtin_function_fragment.declare("ascii_encode", 1, 1).function;
    f.append<AsciiEncode>();
    f.append<jasmin::Return>();
    return &f;
  }());
  return **f;
}

IrFunction const &AsciiDecodeFn() {
  static nth::NoDestructor<IrFunction const *> f([]() -> IrFunction const * {
    auto &f = builtin_function_fragment.declare("ascii_decode", 1, 1).function;
    f.append<AsciiDecode>();
    f.append<jasmin::Return>();
    return &f;
  }());
  return **f;
}

IrFunction const &Foreign() {
  static nth::NoDestructor<IrFunction const *> f([]() -> IrFunction const * {
    auto &f = builtin_function_fragment.declare("foreign", 3, 1).function;
    f.append<RegisterForeignFunction>();
    f.append<jasmin::Return>();
    return &f;
  }());
  return **f;
}

IrFunction const &Opaque() {
  static nth::NoDestructor<IrFunction const *> f([]() -> IrFunction const * {
    auto &f = builtin_function_fragment.declare("opaque", 0, 1).function;
    f.append<ConstructOpaqueType>();
    f.append<jasmin::Return>();
    return &f;
  }());
  return **f;
}

IrFunction const &Arguments() {
  static nth::NoDestructor<IrFunction const *> f([]() -> IrFunction const * {
    auto &f = builtin_function_fragment.declare("arguments", 0, 2).function;
    f.append<LoadProgramArguments>();
    f.append<jasmin::Return>();
    return &f;
  }());
  return **f;
}

IrFunction const &Slice() {
  static nth::NoDestructor<IrFunction const *> f([]() -> IrFunction const * {
    auto &f = builtin_function_fragment.declare("slice", 2, 2).function;
    f.append<jasmin::Return>();
    return &f;
  }());
  return **f;
}

nth::NoDestructor<std::vector<std::string>> BuiltinNamesImpl;

}  // namespace

std::span<std::string const> BuiltinNames() { return *BuiltinNamesImpl; }

Module BuiltinModule() {
  uint32_t next_id = 0;
  Module m;

  auto Register = [&](std::string_view name, type::Type t,
                      IrFunction const &f) {
    m.Insert(Identifier(name), AnyValue(t, &f));
    shared_context.registry.register_function(builtin_function_fragment, f);
    global_function_registry.Register(
        FunctionId(ModuleId::Builtin(), LocalFunctionId(next_id++)), &f);
    BuiltinNamesImpl->emplace_back(name);
  };

  Register("opaque",
           type::Function(
               type::Parameters(std::vector<type::ParametersType::Parameter>{}),
               {type::Type_}),
           Opaque());

  Register("arguments",
           type::Function(
               type::Parameters(std::vector<type::ParametersType::Parameter>{}),
               {type::Slice(type::Slice(type::Char))}),
           Arguments());

  Register("ascii_encode",
           type::Function(
               type::Parameters(std::vector<type::ParametersType::Parameter>{
                   {.type = type::U8}}),
               {type::Char}),
           AsciiEncodeFn());

  Register("ascii_decode",
           type::Function(
               type::Parameters(std::vector<type::ParametersType::Parameter>{
                   {.type = type::Char}}),
               {type::U8}),
           AsciiDecodeFn());

  Register("slice",
           type::Function(
               type::Parameters(std::vector<type::ParametersType::Parameter>{
                   {.type = type::BufPtr(type::Char)}, {.type = type::U64}}),
               {type::Slice(type::Char)}),
           Slice());

  Register("foreign",
           type::Dependent(
               type::DependentTerm::Function(
                   type::DependentTerm::Value(AnyValue(
                       type::Type_,
                       {type::Refinement(type::Type_,
                                         Pattern(&FunctionOrPointer()))})),
                   type::DependentTerm::DeBruijnIndex(0)),
               type::DependentParameterMapping(
                   {type::DependentParameterMapping::Index::Value(1)})),
           Foreign());
  return m;
}

jasmin::ProgramFragment<InstructionSet> const &BuiltinFunctionFragment() {
  return builtin_function_fragment;
}

}  // namespace ic
