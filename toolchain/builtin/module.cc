#include "toolchain/builtin/module.h"

#include <string_view>

#include "common/any_value.h"
#include "common/identifier.h"
#include "common/pattern.h"
#include "common/type.h"

namespace ic::builtin {
namespace {

IrFunction const &PopulateFunctionOrPointer(ProgramFragment &fragment) {
  auto &f = fragment.declare("~function_or_pointer", 1, 1).function;
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
  return f;
}

IrFunction const &PopulateInitializer(ProgramFragment &fragment) {
  auto &f = fragment.declare("~", 0, 0).function;
  f.append<jasmin::Return>();
  return f;
}

IrFunction const &PopulateArguments(ProgramFragment &fragment) {
  auto &f = fragment.declare("arguments", 0, 2).function;
  f.append<LoadProgramArguments>();
  f.append<jasmin::Return>();
  return f;
}

IrFunction const &PopulateAsciiDecode(ProgramFragment &fragment) {
  auto &f = fragment.declare("ascii_decode", 1, 1).function;
  f.append<AsciiDecode>();
  f.append<jasmin::Return>();
  return f;
}

IrFunction const &PopulateAsciiEncode(ProgramFragment &fragment) {
  auto &f = fragment.declare("ascii_encode", 1, 1).function;
  f.append<AsciiEncode>();
  f.append<jasmin::Return>();
  return f;
}

IrFunction const &PopulateOpaque(ProgramFragment &fragment) {
  auto &f = fragment.declare("opaque", 0, 1).function;
  f.append<ConstructOpaqueType>();
  f.append<jasmin::Return>();
  return f;
}

IrFunction const &PopulateSlice(ProgramFragment &fragment) {
  auto &f = fragment.declare("slice", 2, 2).function;
  f.append<jasmin::Return>();
  return f;
}

// IrFunction const &PopulateForeign(ProgramFragment &fragment) {
//   auto &f = fragment.declare("foreign", 3, 1).function;
//   f.append<RegisterForeignFunction>();
//   f.append<jasmin::Return>();
//   return f;
// }

}  // namespace

void PopulateModule(Module &builtin) {
  auto Register = [&](std::string_view name, type::Type t,
                      IrFunction const &(*populate)(
                          ProgramFragment &)) -> IrFunction const & {
    auto const &f = populate(builtin.program());
    builtin.Insert(Identifier(name), AnyValue(t, &f));
    shared_context.registry.register_function(builtin.program(), f);
    return f;
  };

  auto &function_or_pointer = Register(
      "~function_or_pointer",
      type::Function(type::Parameters({{.type = type::Char}}), {type::Bool}),
      PopulateFunctionOrPointer);

  Register("~", type::Function(type::Parameters({}), {}), PopulateInitializer);

  Register("arguments",
           type::Function(type::Parameters({}),
                          {type::Slice(type::Slice(type::Char))}),
           PopulateArguments);

  Register("ascii_decode",
           type::Function(type::Parameters({{.type = type::Char}}), {type::U8}),
           PopulateAsciiDecode);

  Register("ascii_encode",
           type::Function(type::Parameters({{.type = type::U8}}), {type::Char}),
           PopulateAsciiEncode);

  Register("opaque", type::Function(type::Parameters({}), {type::Type_}),
           PopulateOpaque);

  Register("slice",
           type::Function(type::Parameters({{.type = type::BufPtr(type::Char)},
                                            {.type = type::U64}}),
                          {type::Slice(type::Char)}),
           PopulateSlice);

  // Register("foreign",
  //          type::Dependent(
  //              type::DependentTerm::Function(
  //                  type::DependentTerm::Value(AnyValue(
  //                      type::Type_,
  //                      {type::Refinement(type::Type_,
  //                                        Pattern(&function_or_pointer))})),
  //                  type::DependentTerm::DeBruijnIndex(0)),
  //              type::DependentParameterMapping(
  //                  {type::DependentParameterMapping::Index::Value(1)})),
  //          PopulateForeign);
}

}  // namespace ic::builtin
