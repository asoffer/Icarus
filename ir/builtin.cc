#include "ir/builtin.h"

#include "base/debug.h"
#include "ir/any_func.h"
#include "ir/compiled_fn.h"
#include "type/type.h"

namespace ir {

type::Type const* BuiltinType(Builtin b) {
  switch (b) {
#define IR_BUILTIN_MACRO(enumerator, str, t)                                   \
  case Builtin::enumerator:                                                    \
    return t;
#include "ir/builtin.xmacro.h"
#undef IR_BUILTIN_MACRO
  }
  UNREACHABLE();
}

std::string stringify(Builtin b) {
  switch (b) {
#define IR_BUILTIN_MACRO(enumerator, str, ...)                                 \
  case Builtin::enumerator:                                                    \
    return str;
#include "ir/builtin.xmacro.h"
#undef IR_BUILTIN_MACRO
  }
  UNREACHABLE();
}

AnyFunc DebugIrFn() {
  static CompiledFn *debug_ir_func_ = []() {
    auto fn = new CompiledFn(nullptr, type::Func({}, {}),
                       core::FnParams<type::Typed<ast::Expression *>>{});
    CURRENT_FUNC(fn) {
      BasicBlock::Current = fn->entry();
      DebugIr();
      ReturnJump();
    }
    return fn;
  }();
  return AnyFunc{debug_ir_func_};
}

AnyFunc BytesFn() {
  static CompiledFn *bytes_func_ = [&]() {
    auto fn = new CompiledFn(
        nullptr, type::Func({type::Type_}, {type::Int64}),
        core::FnParams(core::Param(
            "", type::Typed<ast::Expression *>(nullptr, type::Type_))));
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
            "", type::Typed<ast::Expression *>(nullptr, type::Type_))));

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
