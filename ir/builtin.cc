#include "ir/builtin.h"

#include "base/debug.h"
#include "ir/any_func.h"
#include "ir/func.h"
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
  static Func *debug_ir_func_ = []() {
    auto fn = new Func(nullptr, type::Func({}, {}),
                       core::FnParams<ast::Expression *>{});
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
  static Func *bytes_func_ = [&]() {
    core::FnParams<ast::Expression *> params;
    params.append("", nullptr);
    auto fn = new Func(nullptr, type::Func({type::Type_}, {type::Int64}),
                       std::move(params));
    CURRENT_FUNC(fn) {
      BasicBlock::Current = fn->entry();
      SetRet(0, Bytes(fn->Argument(0)));
      ReturnJump();
    }
    return fn;
  }();
  return AnyFunc{bytes_func_};
}

AnyFunc AlignmentFn() {
  static Func *bytes_func_ = [&]() {
    core::FnParams<ast::Expression *> params;
    params.append("", nullptr);
    auto fn = new Func(nullptr, type::Func({type::Type_}, {type::Int64}),
                       std::move(params));
    CURRENT_FUNC(fn) {
      BasicBlock::Current = fn->entry();
      SetRet(0, Align(fn->Argument(0)));
      ReturnJump();
    }
    return fn;
  }();
  return AnyFunc{bytes_func_};
}

}  // namespace ir
