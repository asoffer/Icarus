#ifndef ICARUS_IR_INTERPRETER_FFI_H
#define ICARUS_IR_INTERPRETER_FFI_H

#include <ffi.h>

#include "absl/types/span.h"
#include "ir/value/addr.h"
#include "ir/value/result_buffer.h"
#include "type/callable.h"
#include "type/type.h"

namespace ir::interpreter {
namespace internal_ffi {

template <typename>
struct FunctionPointerImpl : std::false_type {};
template <typename ReturnType, typename... Parameters>
struct FunctionPointerImpl<ReturnType (*)(Parameters...)> : std::true_type {};
template <typename T>
concept FunctionPointer = FunctionPointerImpl<T>::value;

bool InvokeForeignFunctionImpl(type::ReturningType const &type, void (*fn)(),
                               CompleteResultBuffer const &arguments,
                               addr_t return_slot);

}  // namespace internal_ffi

// Invokes the function pointer with `arguments` assuming its type is given by
// `type`. The return value is written to `return_slot` if one exists, otherwise
// `return_slot is ignored. A bool is returned indicating whether the function
// was called successfully.
bool InvokeForeignFunction(type::ReturningType const &type,
                           internal_ffi::FunctionPointer auto f,
                           CompleteResultBuffer const &arguments,
                           void *return_slot) {
  return internal_ffi::InvokeForeignFunctionImpl(
      type, reinterpret_cast<void (*)()>(f), arguments,
      static_cast<addr_t>(return_slot));
}

}  // namespace ir::interpreter

#endif  // ICARUS_IR_INTERPRETER_FFI_H
