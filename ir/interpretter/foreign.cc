#include "ir/interpretter/foreign.h"

#include <dlfcn.h>
#include <ffi.h>
#include <tuple>
#include <type_traits>
#include <vector>

#include "base/debug.h"
#include "ir/read_only_data.h"
#include "ir/value/value.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/tuple.h"

namespace interpretter {
using void_fn_ptr = void (*)();

constexpr int kMaxSize = ir::Value::value_size_v;

namespace {
template <typename T>
void ExtractReturnValue(ffi_arg *ret, ir::Addr ret_addr) {
  // libffi promotes return values of size wider than the system register size.
  // size.
  if constexpr (std::is_integral_v<T> and sizeof(T) < sizeof(int)) {
    int int_val;
    std::memcpy(&int_val, ret, sizeof(int_val));
    T val = int_val;
    std::memcpy(ret_addr.heap(), &val, sizeof(val));
  } else {
    std::memcpy(ret_addr.heap(), ret, sizeof(T));
  }
}

ffi_type *ToFfiType(type::Type t) {
  if (t == type::Void()) { return &ffi_type_void; }
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
  if (t.is<type::Pointer>()) { return &ffi_type_pointer; }
  UNREACHABLE();
}
}  // namespace

// TODO return slot is always small enough that we should be able to use a
// stack-allocated buffer for this.
void CallFn(ir::ForeignFn f, base::untyped_buffer const &arguments,
            absl::Span<ir::Addr const> return_slots,
            base::untyped_buffer_view stack) {
  type::Function const *fn_type = f.type();

  std::vector<ffi_type *> arg_types;
  arg_types.reserve(fn_type->params().size());

  std::vector<void *> arg_vals;
  arg_vals.reserve(fn_type->params().size());

  // Note: libffi expects a void*[] for it's arguments but we can't just take
  // pointers into `arguments` when the arguments are in a different format
  // (e.g., when they are pointers and tehrefore stored as ir::Addr rather than
  // void *). So we extract those values appropriately and store them here so
  // that we can take a pointer into `pointer_values`.
  std::vector<void const *> pointer_values;

  size_t i = 0;
  for (auto const &in : fn_type->params()) {
    ASSERT(in.value.constant() == false);
    auto ffi_type = ToFfiType(in.value.type());
    arg_types.push_back(ffi_type);

    // This is more than we need to reserve, but it's sufficient to ensure that
    // push_back will never cause a reallocation so the pointers we take to
    // elements are stable.
    pointer_values.reserve(fn_type->params().size());
    if (ffi_type == &ffi_type_pointer) {
      ir::Addr addr = arguments.get<ir::Addr>(kMaxSize * i);
      LOG("CallFn", "Pushing pointer addr = %s stored in argument %u", addr, i);
      ++i;
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
      arg_vals.push_back(const_cast<char *>(arguments.raw(kMaxSize * i++)));
    }
  }

  ASSERT(fn_type->output().size() <= 1u);

  auto out_type =
      fn_type->output().empty() ? type::Void() : fn_type->output()[0];

  ffi_cif cif;
  ffi_arg ret;

  // TODO this might fail and we need to figure out how to catch that.
  auto prep_result = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, arg_types.size(),
                                  ToFfiType(out_type), arg_types.data());
  LOG("foreign-errno", "before: %d", errno);
  ASSERT(prep_result == FFI_OK);
  ffi_call(&cif, f.get(), &ret, arg_vals.data());
  LOG("foreign-errno", "after: %d", errno);

  if (out_type == type::Void()) {
    goto done;
  } else if (out_type == type::Int8) {
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
  } else if (out_type.is<type::Pointer>()) {
    char *ptr;
    std::memcpy(&ptr, &ret, sizeof(ptr));
    ir::Addr addr;
    uintptr_t ptr_int    = reinterpret_cast<uintptr_t>(ptr);
    uintptr_t stack_head = reinterpret_cast<uintptr_t>(stack.raw(0));
    uintptr_t stack_end =
        reinterpret_cast<uintptr_t>(stack.raw(0)) + stack.size();
    if (stack_head <= ptr_int and ptr_int < stack_end) {
      addr = ir::Addr::Stack(ptr_int - stack_head);
    } else {
      // TODO read-only data?
      addr = ir::Addr::Heap(ptr);
    }
    std::memcpy(return_slots[0].heap(), &addr, sizeof(addr));

  } else {
    NOT_YET();
  }
done:;
}

base::expected<void *> LoadDataSymbol(std::string_view name) {
  dlerror();  // Clear previous errors.
  void *result    = dlsym(RTLD_DEFAULT, std::string(name).c_str());
  char const *err = dlerror();
  if (not err) { return result; }
  return base::unexpected(err);
}

base::expected<void_fn_ptr> LoadFunctionSymbol(std::string_view name) {
  dlerror();  // Clear previous errors.
  auto result = reinterpret_cast<void_fn_ptr>(
      dlsym(RTLD_DEFAULT, std::string(name).c_str()));
  char const *err = dlerror();
  if (not err) { return result; }
  return base::unexpected(err);
}

}  // namespace interpretter
