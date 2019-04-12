#ifndef ICARUS_TYPE_ARRAY_H
#define ICARUS_TYPE_ARRAY_H

#include "base/lazy.h"
#include "type/type.h"

struct Context;

namespace type {

struct Array : public Type {
  TYPE_FNS(Array);
  Array(size_t l, Type const *t) : len(l), data_type(t) {}

  void EmitDestroy(ir::Reg reg, Context *ctx) const override;

  static ir::Results Compare(Array const *lhs_type, ir::Results const &lhs_ir,
                             Array const *rhs_type, ir::Results const &rhs_ir,
                             bool equality, Context *ctx);

  bool IsCopyable() const override;
  bool IsMovable() const override;
  virtual bool needs_destroy() const { return data_type->needs_destroy(); }

  size_t len;
  Type const *data_type;

  base::lazy<ir::Func *> copy_assign_func_;
  base::lazy<ir::Func *> move_assign_func_;
  base::lazy<ir::Func *> init_func_;
  base::lazy<ir::Func *> destroy_func_;
  base::lazy<ir::Func *> repr_func_;
};

Array const *Arr(size_t len, Type const *t);

}  // namespace type
#endif  // ICARUS_TYPE_ARRAY_H
