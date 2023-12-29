#include "ir/function.h"

#include <ffi.h>

#include <deque>

#include "common/slice.h"
#include "ir/foreign_function.h"
#include "ir/function_id.h"
#include "ir/global_function_registry.h"
#include "ir/program_arguments.h"
#include "jasmin/core/value.h"
#include "type/function.h"
#include "type/primitive.h"

namespace ic {
namespace {

[[maybe_unused]] ffi_type* FfiType(type::Type t) {
  if (t == type::Char) {
    return std::is_signed_v<char> ? &ffi_type_schar : &ffi_type_uchar;
  }
  if (t == type::I8) { return &ffi_type_sint8; }
  if (t == type::I16) { return &ffi_type_sint16; }
  if (t == type::I32) { return &ffi_type_sint32; }
  if (t == type::I64) { return &ffi_type_sint64; }
  if (t == type::U8) { return &ffi_type_uint8; }
  if (t == type::U16) { return &ffi_type_uint16; }
  if (t == type::U32) { return &ffi_type_uint32; }
  if (t == type::U64) { return &ffi_type_uint64; }
  if (t == type::F32) { return &ffi_type_float; }
  if (t == type::F64) { return &ffi_type_double; }
  return &ffi_type_pointer;
}

template <typename T>
T Read(ffi_arg const& value) {
  T result;
  std::memcpy(&result, &value, sizeof(T));
  return result;
}

}  // namespace

std::array<jasmin::Value, 2> LoadProgramArguments::execute(
    std::span<jasmin::Value, 0>) {
  Slice arguments = ProgramArguments();
  return {jasmin::Value(arguments.data()), jasmin::Value(arguments.count())};
}

jasmin::Value RegisterForeignFunction::consume(std::span<jasmin::Value, 3> inputs) {
  char const* data = inputs[0].as<char const*>();
  size_t length    = inputs[1].as<size_t>();
  type::Type t     = inputs[2].as<type::Type>();
  if (t.kind() == type::Type::Kind::Function) {
    auto const& f = InsertForeignFunction(std::string_view(data, length),
                                          t.AsFunction(), false);
    return &f;
  } else {
    return InsertForeignPointer(std::string_view(data, length), t.AsPointer());
  }
}

void InvokeForeignFunction::consume(std::span<jasmin::Value> input,
                                    std::span<jasmin::Value> output,
                                    type::FunctionType type,
                                    void const* fn_ptr) {
  std::span returns      = type.returns();
  auto const& parameters = *type.parameters();
  auto parameter_iter    = parameters.begin();
  NTH_REQUIRE((v.debug), returns.size() <= 1);

  ffi_cif call_interface;

  ffi_type* return_type =
      returns.empty() ? &ffi_type_void : FfiType(returns[0]);
  std::vector<ffi_type*> argument_types;
  std::vector<void*> argument_values;
  argument_types.reserve(parameters.size());
  argument_values.reserve(parameters.size());

  for (auto iter = input.begin(); iter != input.end();
       ++iter, ++parameter_iter) {
    argument_types.push_back(FfiType(parameter_iter->type));
    argument_values.push_back(static_cast<void*>(&*iter));
  }

  ffi_status status =
      ffi_prep_cif(&call_interface, FFI_DEFAULT_ABI, argument_types.size(),
                   return_type, argument_types.data());
  NTH_REQUIRE(status == FFI_OK);

  ffi_arg return_value;
  ffi_call(&call_interface,
           reinterpret_cast<void (*)()>(const_cast<void*>(fn_ptr)),
           &return_value, argument_values.data());

  if (not returns.empty()) {
    type::Type t = returns[0];
    if (t == type::Char) {
      output[0] = Read<char>(return_value);
    } else if (t == type::I8) {
      output[0] = Read<int8_t>(return_value);
    } else if (t == type::I16) {
      output[0] = Read<int16_t>(return_value);
    } else if (t == type::I32) {
      output[0] = Read<int32_t>(return_value);
    } else if (t == type::I64) {
      output[0] = Read<int64_t>(return_value);
    } else if (t == type::U8) {
      output[0] = Read<uint8_t>(return_value);
    } else if (t == type::U16) {
      output[0] = Read<uint16_t>(return_value);
    } else if (t == type::U32) {
      output[0] = Read<uint32_t>(return_value);
    } else if (t == type::U64) {
      output[0] = Read<uint64_t>(return_value);
    } else if (t == type::F32) {
      output[0] = Read<float>(return_value);
    } else if (t == type::F64) {
      output[0] = Read<double>(return_value);
    } else {
      output[0] = Read<char const*>(return_value);
    }
  }
}

type::Type ConstructFunctionType::consume(std::span<jasmin::Value, 2> inputs) {
  auto parameter   = inputs[0].as<type::Type>();
  auto return_type = inputs[1].as<type::Type>();
  std::vector<type::Type> returns;
  if (return_type != type::Bottom) { returns.push_back(return_type); }
  if (parameter.kind() == type::Type::Kind::Parameters) {
    return type::Function(parameter.AsParameters(), std::move(returns));
  } else {
    return type::Function(
        type::Parameters({{.name = Identifier("").value(), .type = parameter}}),
        std::move(returns));
  }
}

}  // namespace ic
