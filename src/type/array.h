#ifndef ICARUS_TYPE_ARRAY_H
#define ICARUS_TYPE_ARRAY_H

#include "type.h"

namespace type {
struct Array : public Type {
  TYPE_FNS(Array);
  Array(const Type *t) : data_type(t), len(0), fixed_length(false) {}
  Array(const Type *t, size_t l)
      : data_type(t), len(l), fixed_length(true) {}

  static IR::Val Compare(const Array *lhs_type, IR::Val lhs_ir,
                         const Array *rhs_type, IR::Val rhs_ir, bool equality);

  virtual bool needs_destroy() const {
    return !fixed_length || data_type->needs_destroy();
  }

  mutable IR::Func *repr_func_ = nullptr, *init_func_ = nullptr;

  const Type *data_type;
  size_t len;
  bool fixed_length;

private:
  mutable std::unordered_map<const Array *, IR::Func *> assign_fns_;
  mutable IR::Func *destroy_func_ = nullptr;
};

const Array *Arr(const Type *t);
const Array *Arr(const Type *t, size_t len);

} // namespace type
#endif // ICARUS_TYPE_ARRAY_H
