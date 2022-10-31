#include "semantic_analysis/instruction_set.h"

#include <ffi.h>

#include "absl/container/inlined_vector.h"
#include "ir/value/fn.h"
#include "jasmin/value.h"
#include "semantic_analysis/foreign_function_map.h"

namespace semantic_analysis {
namespace {

ffi_type* FfiType(core::Type t) {
  if (t == Char) {
    return std::is_signed_v<char> ? &ffi_type_schar : &ffi_type_uchar;
  }
  if (t == I(8)) { return &ffi_type_sint8; }
  if (t == I(16)) { return &ffi_type_sint16; }
  if (t == I(32)) { return &ffi_type_sint32; }
  if (t == I(64)) { return &ffi_type_sint64; }
  if (t == U(8)) { return &ffi_type_uint8; }
  if (t == U(16)) { return &ffi_type_uint16; }
  if (t == U(32)) { return &ffi_type_uint32; }
  if (t == U(64)) { return &ffi_type_uint64; }
  if (t == F32) { return &ffi_type_float; }
  if (t == F64) { return &ffi_type_double; }
  return &ffi_type_pointer;
}

template <jasmin::SmallTrivialValue T>
T Read(ffi_arg const& value) {
  T result;
  std::memcpy(&result, &value, sizeof(T));
  return result;
}

}  // namespace

void BuiltinForeign::execute(jasmin::ValueStack& value_stack, core::Type t,
                             ForeignFunctionMap* foreign_function_map) {
  size_t length        = value_stack.pop<size_t>();
  char const* data     = value_stack.pop<char const*>();
  auto [fn_id, fn_ptr] =
      foreign_function_map->ForeignFunction(std::string(data, length), t);
  if (fn_ptr == nullptr) { NOT_YET(); }
  value_stack.push(fn_ptr);
}

void InvokeForeignFunction::execute(
    jasmin::ValueStack& value_stack, void (*fn_ptr)(),
    core::Parameter<core::Type> const* parameters, size_t parameter_count,
    core::Type const* maybe_return_type) {
  LOG("", "Invoking");
  ffi_cif call_interface;

  ffi_type* return_type =
      maybe_return_type ? FfiType(*maybe_return_type) : &ffi_type_void;

  absl::InlinedVector<ffi_type*, 4> argument_types;
  absl::InlinedVector<void*, 4> argument_values;
  argument_types.reserve(parameter_count);
  argument_values.reserve(parameter_count);

  for (auto iter = value_stack.end() - parameter_count;
       iter != value_stack.end(); ++iter, ++parameters) {
    argument_types.push_back(FfiType(parameters->value));
    argument_values.push_back(const_cast<void*>(iter->address()));
  }

  ffi_status status =
      ffi_prep_cif(&call_interface, FFI_DEFAULT_ABI, argument_types.size(),
                   return_type, argument_types.data());
  ASSERT(status == FFI_OK);

  ffi_arg return_value;
  ffi_call(&call_interface, fn_ptr, &return_value, argument_values.data());

  for (size_t i = 0; i < parameter_count; ++i) { value_stack.pop_value(); }

  if (maybe_return_type) {
    core::Type t = *maybe_return_type;
    if (t == Char) {
      value_stack.push(Read<char>(return_value));
    } else if (t == I(8)) {
      value_stack.push(Read<int8_t>(return_value));
    } else if (t == I(16)) {
      value_stack.push(Read<int16_t>(return_value));
    } else if (t == I(32)) {
      value_stack.push(Read<int32_t>(return_value));
    } else if (t == I(64)) {
      value_stack.push(Read<int64_t>(return_value));
    } else if (t == U(8)) {
      value_stack.push(Read<uint8_t>(return_value));
    } else if (t == U(16)) {
      value_stack.push(Read<uint16_t>(return_value));
    } else if (t == U(32)) {
      value_stack.push(Read<uint32_t>(return_value));
    } else if (t == U(64)) {
      value_stack.push(Read<uint64_t>(return_value));
    } else if (t == F32) {
      value_stack.push(Read<float>(return_value));
    } else if (t == F64) {
      value_stack.push(Read<double>(return_value));
    } else {
      value_stack.push(Read<char const*>(return_value));
    }
  }
}

}  // namespace semantic_analysis
