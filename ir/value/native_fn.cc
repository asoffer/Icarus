#include "ir/value/native_fn.h"

namespace ir {

NativeFn::NativeFn(NativeFn::Data *data) : data_(data) {}

type::Function const *NativeFn::type() const { return data_->type(); }

}  // namespace ir
