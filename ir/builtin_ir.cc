#include "ir/builtin_ir.h"

#include "core/params.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "type/function.h"
#include "type/typed_value.h"

namespace ir {

CompiledFn *BytesFn() {
  static CompiledFn *bytes_func_ = [&]() {
    auto const *fn_type =
        type::Func({core::AnonymousParam(type::Type_)}, {type::Int64});
    auto fn = new CompiledFn(fn_type, fn_type->AnonymousParams());
    ICARUS_SCOPE(SetCurrent(fn)) {
      auto &bldr          = GetBuilder();
      bldr.CurrentBlock() = fn->entry();
      SetRet(0, bldr.Bytes(Reg::Arg(0)));
      bldr.ReturnJump();
    }
    return fn;
  }();
  return bytes_func_;
}

CompiledFn *AlignmentFn() {
  static CompiledFn *alignment_func_ = [&]() {
    auto const *fn_type =
        type::Func({core::AnonymousParam(type::Type_)}, {type::Int64});
    auto fn = new CompiledFn(fn_type, fn_type->AnonymousParams());
    ICARUS_SCOPE(SetCurrent(fn)) {
      auto &bldr          = GetBuilder();
      bldr.CurrentBlock() = fn->entry();
      SetRet(0, bldr.Align(Reg::Arg(0)));
      bldr.ReturnJump();
    }
    return fn;
  }();
  return alignment_func_;
}

}  // namespace ir
