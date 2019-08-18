#include "ir/builtin_ir.h"

#include "core/fn_params.h"
#include "ir/cmd.h"
#include "ir/cmd/jumps.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/set_ret.h"
#include "ir/compiled_fn.h"
#include "type/function.h"
#include "type/typed_value.h"

namespace ir {

AnyFunc BytesFn() {
  static CompiledFn *bytes_func_ = [&]() {
    auto fn = new CompiledFn(
        nullptr, type::Func({type::Type_}, {type::Int64}),
        core::FnParams(core::Param(
            "", type::Typed<ast::Expression const *>(nullptr, type::Type_))));
    CURRENT_FUNC(fn) {
      BasicBlock::Current = fn->entry();
      SetRet(0, Bytes(Reg::Arg(0)));
      ReturnJump();
    }
    return fn;
  }();
  return AnyFunc{bytes_func_};
}

AnyFunc AlignmentFn() {
  static CompiledFn *bytes_func_ = [&]() {
    auto fn = new CompiledFn(
        nullptr, type::Func({type::Type_}, {type::Int64}),
        core::FnParams(core::Param(
            "", type::Typed<ast::Expression const *>(nullptr, type::Type_))));

    CURRENT_FUNC(fn) {
      BasicBlock::Current = fn->entry();
      SetRet(0, Align(Reg::Arg(0)));
      ReturnJump();
    }
    return fn;
  }();
  return AnyFunc{bytes_func_};
}

}  // namespace ir
