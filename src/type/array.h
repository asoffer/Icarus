#ifndef ICARUS_TYPE_ARRAY_H
#define ICARUS_TYPE_ARRAY_H

#include <mutex>
#include "base/container/unordered_map.h"
#include "type.h"

struct Context;

namespace type {
struct Array : public Type {
  TYPE_FNS(Array);
  Array(const Type *t) : data_type(t), len(0), fixed_length(false) {}
  Array(const Type *t, size_t l)
      : data_type(t), len(l), fixed_length(true) {}

  static IR::Val Compare(const Array *lhs_type, IR::Val lhs_ir,
                         const Array *rhs_type, IR::Val rhs_ir, bool equality,
                         Context *ctx);

  virtual bool needs_destroy() const {
    return !fixed_length || data_type->needs_destroy();
  }

  void EmitResize(IR::Val ptr_to_array, IR::Val new_size, Context *ctx) const;

  const Type *data_type;
  size_t len;
  bool fixed_length;

private:
 void ComputeDestroyWithoutLock(Context *ctx) const;
 mutable std::mutex mtx_;
 mutable base::unordered_map<const Array *, IR::Func *> assign_fns_;
 mutable IR::Func *destroy_func_ = nullptr;
 mutable IR::Func *repr_func_    = nullptr;
 mutable IR::Func *init_func_    = nullptr;
 mutable IR::Func *resize_func_  = nullptr;
};

const Array *Arr(const Type *t);
const Array *Arr(const Type *t, size_t len);

} // namespace type
#endif // ICARUS_TYPE_ARRAY_H
