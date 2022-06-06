#include "ffi.h"

#include <type_traits>

#include "absl/container/inlined_vector.h"
#include "base/debug.h"
#include "core/arch.h"
#include "type/pointer.h"
#include "type/primitive.h"

namespace ir::interpreter {
namespace {
constexpr size_t kInlinedArgumentCount = 4;

// Converts a `type::Type` to the corresponding `ffi_type`.
ffi_type *FfiType(type::Type t) {
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
  if (t == type::F64) { return &ffi_type_double; }
  if (t.is<type::Pointer>()) { return &ffi_type_pointer; }
  UNREACHABLE(t);
}
}  // namespace


// Initializes `call_interface`, returning a bool indicating whether or not
// initialization was successful.
bool InitializeCallInterface(type::ReturningType const &type,
                             ffi_cif &call_interface,
                             absl::Span<ffi_type *> argument_types) {
  absl::Span return_types = type.return_types();
  if (return_types.size() > 1) { return false; }

  ffi_type *return_type =
      return_types.empty() ? &ffi_type_void : FfiType(return_types[0]);

  size_t i = 0;
  for (auto const &parameter : type.parameters()) {
    if (parameter.value.quals() != type::Qualifiers::Unqualified()) { return false; }
    argument_types[i++] = FfiType(parameter.value.type());
  }

  ffi_status status =
      ffi_prep_cif(&call_interface, FFI_DEFAULT_ABI, argument_types.size(),
                   return_type, argument_types.data());
  return status == FFI_OK;
}

namespace internal_ffi {

bool InvokeForeignFunctionImpl(type::ReturningType const &type, void (*fn)(),
                           CompleteResultBuffer const &arguments,
                           addr_t return_slot) {
  ASSERT(type.return_types().size() == (return_slot ? 1 : 0));
  size_t count = type.parameters().size();

  ffi_cif call_interface;

  // `argument_types` is required to be validat least until `ffi_call` is
  // invoked.
  absl::InlinedVector<ffi_type *, kInlinedArgumentCount> argument_types(count);

  if (not InitializeCallInterface(type, call_interface,
                                  absl::MakeSpan(argument_types))) {
    return false;
  }

  absl::InlinedVector<void *, kInlinedArgumentCount> argument_values;
  argument_values.reserve(count);
  for (size_t i = 0; i < count; ++i) {
    argument_values.push_back(const_cast<addr_t>(arguments[i].raw().data()));
  }

  ffi_arg return_value;
  ffi_call(&call_interface, fn, &return_value, argument_values.data());
  if (return_slot) {
    std::memcpy(return_slot, &return_value,
                type.return_types()[0].bytes(core::Host).value());
  }
  return true;
}

}  // namespace internal_ffi

}  // namespace
