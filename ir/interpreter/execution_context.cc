#include "ir/interpreter/execution_context.h"

#include <dlfcn.h>
#include <ffi.h>

#include "type/pointer.h"

namespace interpreter {
namespace {

template <typename T>
void ExtractReturnValue(ExecutionContext& ctx, ffi_arg *ret, StackFrame &frame) {
  // libffi promotes return values of size wider than the system register size.
  // size.
  static constexpr bool PromotesToInt =
      std::is_integral_v<T> and sizeof(T) < sizeof(int);
  using ffi_ret_type = std::conditional_t<PromotesToInt, int, T>;
  ffi_ret_type value;
  std::memcpy(&value, ret, sizeof(value));
  ctx.Store(frame.regs_.get<ir::Addr>(ir::Reg::Out(0)), static_cast<T>(value));
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
void ExecutionContext::CallFn(ir::ForeignFn f, StackFrame &frame,
                              base::untyped_buffer_view stack) {
  type::Function const *fn_type = f.type();
  LOG("CallFn", "Calling %s: %s", f, fn_type->to_string());

  std::vector<ffi_type *> arg_types;
  arg_types.reserve(fn_type->params().size());

  std::vector<void *> arg_vals;
  arg_vals.reserve(fn_type->params().size());

  // Note: libffi expects a void*[] for its arguments but we can't just take
  // pointers into `frame` when the arguments are in a different format (e.g.,
  // when they are pointers and therefore stored as ir::Addr rather than
  // void*). So we extract those values appropriately and store them here so
  // that we can take a pointer into `pointer_values`.
  std::vector<void const *> pointer_values;

  for (size_t i = 0; i < fn_type->params().size(); ++i) {
    auto const& in = fn_type->params()[i];

    ASSERT(in.value.constant() == false);
    auto ffi_type = ToFfiType(in.value.type());
    arg_types.push_back(ffi_type);

    // This is more than we need to reserve, but it's sufficient to ensure that
    // push_back will never cause a reallocation so the pointers we take to
    // elements are stable.
    pointer_values.reserve(fn_type->params().size());
    if (ffi_type == &ffi_type_pointer) {
      ir::Addr addr = frame.regs_.get<ir::Addr>(ir::Reg::Arg(i));

      LOG("CallFn", "Pushing pointer addr = %s stored in %s", addr,
          ir::Reg::Arg(i));
      switch (addr.kind()) {
        case ir::Addr::Kind::Heap: {
          pointer_values.push_back(addr.heap());
        } break;
        case ir::Addr::Kind::Stack: {
          pointer_values.push_back(stack.raw(addr.stack()));
        } break;
        case ir::Addr::Kind::ReadOnly: {
          void *ptr = ir::ReadOnlyData.lock()->raw(addr.rodata());
          pointer_values.push_back(ptr);
        } break;
      }
      arg_vals.push_back(&pointer_values.back());
    } else {
      // TODO: This is sufficient for integer types where we've written the
      // values directly into the buffer. Detetrmine if this is also okay for
      // ir::Char where we're writing/reading `char` through the `ir::Char`
      // according to the C++ standard.
      arg_vals.push_back(
          const_cast<char *>(frame.regs_.raw(ir::Reg::Arg(i))));
    }
  }

  ASSERT(fn_type->output().size() <= 1u);

  auto out_type = fn_type->output().empty() ? type::Void : fn_type->output()[0];

  ffi_cif cif;
  ffi_arg ret;

  // TODO this might fail and we need to figure out how to catch that.
  auto prep_result = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, arg_types.size(),
                                  ToFfiType(out_type), arg_types.data());
  LOG("foreign-errno", "before: %d", errno);
  ASSERT(prep_result == FFI_OK);
  ffi_call(&cif, f.get(), &ret, arg_vals.data());
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
    char *ptr;
    std::memcpy(&ptr, &ret, sizeof(ptr));
    ir::Addr addr;
    uintptr_t ptr_int    = reinterpret_cast<uintptr_t>(ptr);
    uintptr_t stack_head = reinterpret_cast<uintptr_t>(stack.raw(0));
    uintptr_t stack_end  = stack_head + stack.size();
    if (stack_head <= ptr_int and ptr_int < stack_end) {
      addr = ir::Addr::Stack(ptr_int - stack_head);
    } else {
      // TODO: read-only data?
      addr = ir::Addr::Heap(ptr);
    }

    Store(frame.regs_.get<ir::Addr>(ir::Reg::Out(0)), addr);
  } else {
    UNREACHABLE(out_type);
  }
}

void ExecutionContext::CallFn(ir::BuiltinFn fn, StackFrame &frame) {
  switch (fn.which()) {
    case ir::BuiltinFn::Which::Alignment: {
      type::Type type = frame.regs_.get<type::Type>(ir::Reg::Arg(0));
      auto out        = frame.regs_.get<ir::Addr>(ir::Reg::Out(0));
      *static_cast<uint64_t *>(ASSERT_NOT_NULL(out.heap())) =
          type.alignment(kArchitecture).value();
    } break;
    case ir::BuiltinFn::Which::Bytes: {
      type::Type type = frame.regs_.get<type::Type>(ir::Reg::Arg(0));
      auto out        = frame.regs_.get<ir::Addr>(ir::Reg::Out(0));
      *static_cast<uint64_t *>(ASSERT_NOT_NULL(out.heap())) =
          type.bytes(kArchitecture).value();
    } break;
    default: NOT_YET();
  }

}

void ExecutionContext::Load(ir::Reg result, ir::Addr addr,
                            core::Bytes num_bytes) {
  switch (addr.kind()) {
    case ir::Addr::Kind::Stack: {
      current_frame().regs_.set_raw(result, stack_.raw(addr.stack()),
                                    num_bytes.value());
    } break;
    case ir::Addr::Kind::ReadOnly: {
      auto handle = ir::ReadOnlyData.lock();
      current_frame().regs_.set_raw(result, handle->raw(addr.rodata()),
                                    num_bytes.value());
    } break;
    case ir::Addr::Kind::Heap: {
      current_frame().regs_.set_raw(result, addr.heap(), num_bytes.value());
    } break;
  }
}

}  // namespace interpreter
