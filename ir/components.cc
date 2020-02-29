#include "ir/components.h"

#include "ir/builder.h"

namespace ir {

base::Tagged<Addr, Reg> Index(type::Pointer const *t, Reg array_ptr,
                              RegOr<int64_t> offset) {
  // TODO this works but generates worse ir (both here and in llvm). It's worth
  // figuring out how to do this better. Is this still true without
  // variable-length arrays?
  return GetBuilder().PtrIncr(
      array_ptr, offset, type::Ptr(t->pointee->as<type::Array>().data_type));
}

}  // namespace ir
