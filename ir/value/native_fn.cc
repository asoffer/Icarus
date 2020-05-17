#include "ir/value/native_fn.h"

namespace ir {

NativeFn::NativeFn(CompiledFn *fn) : fn_(ASSERT_NOT_NULL(fn)) {}

NativeFn::NativeFn(NativeFnSet *set, type::Function const *fn_type,
                   core::Params<type::Typed<ast::Declaration const *>> p)
    : NativeFn(
          set->fns
              .emplace_back(std::make_unique<CompiledFn>(fn_type, std::move(p)))
              .get()) {}

type::Function const *NativeFn::type() const { return get()->type(); }

CompiledFn *NativeFn::get() const { return fn_; }

}  // namespace ir
