#include "ir/builtin_ir.h"

#include "core/fn_params.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "type/function.h"
#include "type/typed_value.h"

namespace ir {

AnyFunc BytesFn() {
  static CompiledFn *bytes_func_ = [&]() {
    auto const *fn_type = type::Func({type::Type_}, {type::Int64});
    auto fn             = new CompiledFn(fn_type, fn_type->AnonymousFnParams());
    ICARUS_SCOPE(SetCurrent(fn)) {
      auto &bldr          = GetBuilder();
      bldr.CurrentBlock() = fn->entry();
      SetRet(0, bldr.Bytes(Reg::Arg(0)));
      bldr.ReturnJump();
    }
    return fn;
  }();
  return AnyFunc{bytes_func_};
}

AnyFunc AlignmentFn() {
  static CompiledFn *alignment_func_ = [&]() {
    auto const *fn_type = type::Func({type::Type_}, {type::Int64});
    auto fn             = new CompiledFn(fn_type, fn_type->AnonymousFnParams());
    ICARUS_SCOPE(SetCurrent(fn)) {
      auto &bldr          = GetBuilder();
      bldr.CurrentBlock() = fn->entry();
      SetRet(0, bldr.Align(Reg::Arg(0)));
      bldr.ReturnJump();
    }
    return fn;
  }();
  return AnyFunc{alignment_func_};
}

}  // namespace ir
