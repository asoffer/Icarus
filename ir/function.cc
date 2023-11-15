#include "ir/function.h"

#include <ffi.h>

#include <deque>

#include "ir/foreign_function.h"
#include "ir/function_id.h"
#include "ir/global_function_registry.h"
#include "jasmin/value.h"
#include "jasmin/value_stack.h"

namespace ic {
namespace {

ffi_type* FfiType(type::Type t) {
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

template <jasmin::SmallTrivialValue T>
T Read(ffi_arg const& value) {
  T result;
  std::memcpy(&result, &value, sizeof(T));
  return result;
}

}  // namespace

void RegisterForeignFunction::execute(jasmin::ValueStack& value_stack) {
  type::Type t     = value_stack.pop<type::Type>();
  size_t length    = value_stack.pop<size_t>();
  char const* data = value_stack.pop<char const*>();
  auto const& f    = InsertForeignFunction(std::string_view(data, length),
                                           t.AsFunction(), false);
  value_stack.push(&f);
}

void InvokeForeignFunction::execute(jasmin::ValueStack& value_stack,
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

  for (auto iter = value_stack.end() - parameters.size();
       iter != value_stack.end(); ++iter, ++parameter_iter) {
    argument_types.push_back(FfiType(parameter_iter->type));
    argument_values.push_back(const_cast<void*>(iter->address()));
  }

  ffi_status status =
      ffi_prep_cif(&call_interface, FFI_DEFAULT_ABI, argument_types.size(),
                   return_type, argument_types.data());
  NTH_REQUIRE(status == FFI_OK);

  ffi_arg return_value;
  ffi_call(&call_interface,
           reinterpret_cast<void (*)()>(const_cast<void*>(fn_ptr)),
           &return_value, argument_values.data());

  for (size_t i = 0; i < parameters.size(); ++i) { value_stack.pop_value(); }

  if (not returns.empty()) {
    type::Type t = returns[0];
    if (t == type::Char) {
      value_stack.push(Read<char>(return_value));
    } else if (t == type::I8) {
      value_stack.push(Read<int8_t>(return_value));
    } else if (t == type::I16) {
      value_stack.push(Read<int16_t>(return_value));
    } else if (t == type::I32) {
      value_stack.push(Read<int32_t>(return_value));
    } else if (t == type::I64) {
      value_stack.push(Read<int64_t>(return_value));
    } else if (t == type::U8) {
      value_stack.push(Read<uint8_t>(return_value));
    } else if (t == type::U16) {
      value_stack.push(Read<uint16_t>(return_value));
    } else if (t == type::U32) {
      value_stack.push(Read<uint32_t>(return_value));
    } else if (t == type::U64) {
      value_stack.push(Read<uint64_t>(return_value));
    } else if (t == type::F32) {
      value_stack.push(Read<float>(return_value));
    } else if (t == type::F64) {
      value_stack.push(Read<double>(return_value));
    } else {
      value_stack.push(Read<char const*>(return_value));
    }
  }
}

}  // namespace ic
