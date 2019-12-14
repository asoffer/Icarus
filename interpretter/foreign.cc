#include "interpretter/foreign.h"

#include <dlfcn.h>
#include <ffi.h>
#include <tuple>
#include <type_traits>
#include <vector>

#include "base/debug.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"

namespace interpretter {
using void_fn_ptr = void (*)();

namespace {
template <typename T>
void ExtractReturnValue(ffi_arg *ret, ir::Addr ret_addr) {
  // libffi promotes return values of size wider than the system register size.
  // size.
  if constexpr (std::is_integral_v<T> and sizeof(T) < sizeof(int)) {
    int int_val;
    std::memcpy(&int_val, ret, sizeof(int));
    T val = int_val;
    std::memcpy(ret_addr.as_heap, &val, sizeof(T));
  } else {
    std::memcpy(ret_addr.as_heap, ret, sizeof(T));
  }
}

ffi_type *ToFfiType(type::Type const *t) {
  if (t == type::Int8) { return &ffi_type_sint8; }
  if (t == type::Int16) { return &ffi_type_sint16; }
  if (t == type::Int32) { return &ffi_type_sint32; }
  if (t == type::Int64) { return &ffi_type_sint64; }
  if (t == type::Nat8) { return &ffi_type_uint8; }
  if (t == type::Nat16) { return &ffi_type_uint16; }
  if (t == type::Nat32) { return &ffi_type_uint32; }
  if (t == type::Nat64) { return &ffi_type_uint64; }
  if (t == type::Float32) { return &ffi_type_float; }
  if (t == type::Float64) { return &ffi_type_double; }
  return nullptr;
}
}  // namespace

// TODO return slot is always small enough that we should be able to use a
// stack-allocated buffer for this.
void CallForeignFn(ir::ForeignFn f, base::untyped_buffer const &arguments,
                   absl::Span<ir::Addr const> return_slots,
                   base::untyped_buffer *stack) {
  type::Function const *fn_type = f.type();

  std::vector<ffi_type *> arg_types;
  arg_types.reserve(fn_type->input.size());

  std::vector<void *> arg_vals;
  arg_vals.reserve(fn_type->input.size());

  size_t i = 0;
  for (type::Type const *in : fn_type->input) {
    arg_types.push_back(ASSERT_NOT_NULL(ToFfiType(in)));
    // TODO if the type is a pointer or something that's not the same between
    // the host and the interpretter, this won't work.
    arg_vals.push_back(const_cast<void *>(arguments.raw(16 * i++)));
  }

  ASSERT(fn_type->output.size() == 1u);
  auto *out_type = fn_type->output[0];

  ffi_cif cif;
  ffi_arg ret;

  // TODO this might fail and we need to figure out how to catch that.
  auto prep_result = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, arg_types.size(),
                                  ToFfiType(out_type), arg_types.data());
  ASSERT(prep_result == FFI_OK);
  ffi_call(&cif, f.get(), &ret, arg_vals.data());

  if (out_type == type::Int8) {
    ExtractReturnValue<int8_t>(&ret, return_slots[0]);
  } else if (out_type == type::Int16) {
    ExtractReturnValue<int16_t>(&ret, return_slots[0]);
  } else if (out_type == type::Int32) {
    ExtractReturnValue<int32_t>(&ret, return_slots[0]);
  } else if (out_type == type::Int64) {
    ExtractReturnValue<int64_t>(&ret, return_slots[0]);
  } else if (out_type == type::Nat8) {
    ExtractReturnValue<uint8_t>(&ret, return_slots[0]);
  } else if (out_type == type::Nat16) {
    ExtractReturnValue<uint16_t>(&ret, return_slots[0]);
  } else if (out_type == type::Nat32) {
    ExtractReturnValue<uint32_t>(&ret, return_slots[0]);
  } else if (out_type == type::Nat64) {
    ExtractReturnValue<uint64_t>(&ret, return_slots[0]);
  } else if (out_type == type::Float32) {
    ExtractReturnValue<float>(&ret, return_slots[0]);
  } else if (out_type == type::Float64) {
    ExtractReturnValue<double>(&ret, return_slots[0]);
  } else if (out_type->is<type::Pointer>()) {
    NOT_YET();
  } else {
    NOT_YET();
  }
}

void *LoadDataSymbol(std::string_view name) {
  return ASSERT_NOT_NULL(dlsym(RTLD_DEFAULT, std::string(name).c_str()));
}

void_fn_ptr LoadFunctionSymbol(std::string_view name) {
  // Note: This cast is in general not safe but valid on POSIX compliant
  // systems.
  // TODO: Figure out a portable way of handling this if at all possible.
  return reinterpret_cast<void_fn_ptr>(
      ASSERT_NOT_NULL(dlsym(RTLD_DEFAULT, std::string(name).c_str())));
}

}  // namespace interpretter
