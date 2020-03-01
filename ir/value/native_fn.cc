#include "ir/value/native_fn.h"

namespace ir {

NativeFn::NativeFn(CompiledFn *fn) : fn_(ASSERT_NOT_NULL(fn)) {}

type::Function const *NativeFn::type() const { return get()->type(); }

CompiledFn *NativeFn::get() const {
  return fn_;
}

}  // namespace ir
