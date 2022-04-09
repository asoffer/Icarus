#include "ir/interpreter/execution_context.h"

#include <dlfcn.h>
#include <ffi.h>

#include "type/pointer.h"
#include "type/primitive.h"

namespace interpreter {
namespace {

template <typename T>
void ExtractReturnValue(ExecutionContext &ctx, ffi_arg *ret,
                        StackFrame &frame) {
  // libffi promotes return values of size wider than the system register size.
  // size.
  static constexpr bool PromotesToInt =
      std::is_integral_v<T> and sizeof(T) < sizeof(int);
  using ffi_ret_type = std::conditional_t<PromotesToInt, int, T>;
  ffi_ret_type value;
  std::memcpy(&value, ret, sizeof(value));
  ctx.Store(frame.get<ir::addr_t>(ir::Reg::Output(0)), static_cast<T>(value));
}

ffi_type *ToFfiType(type::Type t) {
  if (t == type::Void) { return &ffi_type_void; }
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

// TODO return slot is always small enough that we should be able to use a
// stack-allocated buffer for this.
void ExecutionContext::CallForeignFunction(ir::Fn f, StackFrame &frame) {
  type::Function const *fn_type = shared_context_.FunctionType(f);
  LOG("CallFn", "Calling %s: %s", f, fn_type->to_string());

  std::vector<ffi_type *> arg_types;
  arg_types.reserve(fn_type->params().size());

  std::vector<void *> arg_vals;
  arg_vals.reserve(fn_type->params().size());

  // Note: libffi expects a void*[] for its arguments but we can't just take
  // pointers into `frame` when the arguments are in a different format (e.g.,
  // when they are pointers and therefore stored as ir::addr_t rather than
  // void*). So we extract those values appropriately and store them here so
  // that we can take a pointer into `pointer_values`.
  std::vector<void const *> pointer_values;

  for (size_t i = 0; i < fn_type->params().size(); ++i) {
    auto const &in = fn_type->params()[i];

    ASSERT(in.value.constant() == false);
    auto ffi_type = ToFfiType(in.value.type());
    arg_types.push_back(ffi_type);

    // This is more than we need to reserve, but it's sufficient to ensure that
    // push_back will never cause a reallocation so the pointers we take to
    // elements are stable.
    pointer_values.reserve(fn_type->params().size());
    if (ffi_type == &ffi_type_pointer) {
      ir::addr_t addr = frame.get<ir::addr_t>(ir::Reg::Parameter(i));
      LOG("CallFn", "Pushing pointer addr = %p stored in %s", addr,
          ir::Reg::Parameter(i));
      pointer_values.push_back(addr);
      arg_vals.push_back(&pointer_values.back());
    } else {
      // TODO: This is sufficient for integer types where we've written the
      // values directly into the buffer. Detetrmine if this is also okay for
      // ir::Char where we're writing/reading `char` through the `ir::Char`
      // according to the C++ standard.
      arg_vals.push_back(reinterpret_cast<char *>(
          const_cast<std::byte *>(frame.raw(ir::Reg::Parameter(i)))));
    }
  }

  ASSERT(fn_type->return_types().size() <= 1u);

  auto out_type =
      fn_type->return_types().empty() ? type::Void : fn_type->return_types()[0];

  ffi_cif cif;
  ffi_arg ret;

  // TODO this might fail and we need to figure out how to catch that.
  auto prep_result = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, arg_types.size(),
                                  ToFfiType(out_type), arg_types.data());
  LOG("foreign-errno", "before: %d", errno);
  ASSERT(prep_result == FFI_OK);
  ffi_call(&cif, shared_context_.ForeignFunctionPointer(f), &ret,
           arg_vals.data());
  LOG("foreign-errno", "after: %d", errno);

  if (out_type == type::Void) {
    return;
  } else if (out_type == type::I8) {
    ExtractReturnValue<int8_t>(*this, &ret, frame);
  } else if (out_type == type::I16) {
    ExtractReturnValue<int16_t>(*this, &ret, frame);
  } else if (out_type == type::I32) {
    ExtractReturnValue<int32_t>(*this, &ret, frame);
  } else if (out_type == type::I64) {
    ExtractReturnValue<int64_t>(*this, &ret, frame);
  } else if (out_type == type::U8) {
    ExtractReturnValue<uint8_t>(*this, &ret, frame);
  } else if (out_type == type::U16) {
    ExtractReturnValue<uint16_t>(*this, &ret, frame);
  } else if (out_type == type::U32) {
    ExtractReturnValue<uint32_t>(*this, &ret, frame);
  } else if (out_type == type::U64) {
    ExtractReturnValue<uint64_t>(*this, &ret, frame);
  } else if (out_type == type::F32) {
    ExtractReturnValue<float>(*this, &ret, frame);
  } else if (out_type == type::F64) {
    ExtractReturnValue<double>(*this, &ret, frame);
  } else if (out_type.is<type::Pointer>()) {
    ir::addr_t ptr;
    std::memcpy(&ptr, &ret, sizeof(ptr));
    Store(frame.get<ir::addr_t>(ir::Reg::Output(0)), ptr);
  } else {
    UNREACHABLE(out_type);
  }
}

void ExecutionContext::Load(ir::Reg result, ir::addr_t addr,
                            core::Bytes num_bytes) {
  LOG("Load", "%s %p %u", result, addr, num_bytes.value());
  ASSERT(addr != nullptr);
  current_frame().set_raw(result, addr, num_bytes.value());
}

}  // namespace interpreter
