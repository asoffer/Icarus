#ifndef ICARUS_TYPE_ARRAY_H
#define ICARUS_TYPE_ARRAY_H

#include "base/lazy.h"
#include "type/type.h"

namespace type {

struct Array : public Type {
  TYPE_FNS(Array);
  Array(size_t l, Type const *t) : len(l), data_type(t) {}

#include ICARUS_TYPE_VISITOR_METHODS

  size_t len;
  Type const *data_type;

  base::lazy<ir::CompiledFn *> copy_assign_func_;
  base::lazy<ir::CompiledFn *> move_assign_func_;
  base::lazy<ir::CompiledFn *> init_func_;
  base::lazy<ir::CompiledFn *> destroy_func_;
  base::lazy<ir::CompiledFn *> repr_func_;
};

Array const *Arr(size_t len, Type const *t);

}  // namespace type
#endif  // ICARUS_TYPE_ARRAY_H
