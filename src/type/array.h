#ifndef ICARUS_TYPE_ARRAY_H
#define ICARUS_TYPE_ARRAY_H

#include <mutex>
#include "base/container/unordered_map.h"
#include "type.h"

struct Context;

namespace type {

struct Array : public Type {
  TYPE_FNS(Array);
  Array(Type const *t, size_t l) : data_type(t), len(l) {}

  void EmitDestroy(ir::Register reg, Context *ctx) const override;

  static ir::Val Compare(Array const *lhs_type, ir::Val lhs_ir,
                         Array const *rhs_type, ir::Val rhs_ir, bool equality,
                         Context *ctx);

  bool IsCopyable() const override;
  virtual bool needs_destroy() const { return data_type->needs_destroy(); }

  Type const *data_type;
  size_t len;

  mutable std::mutex mtx_;
  mutable base::unordered_map<Array const *, ir::Func *> assign_fns_;
  mutable ir::Func *destroy_func_ = nullptr;
  mutable ir::Func *repr_func_    = nullptr;
  mutable ir::Func *init_func_    = nullptr;
  mutable ir::Func *resize_func_  = nullptr;
};

Array const *Arr(Type const *t, size_t len);

}  // namespace type
#endif  // ICARUS_TYPE_ARRAY_H
