#ifndef ICARUS_TYPE_VARIANT_H
#define ICARUS_TYPE_VARIANT_H

#include "type.h"
#include <mutex>

namespace type {
struct Variant : public Type {
  TYPE_FNS(Variant);
  Variant(base::vector<const Type *> variants)
      : variants_(std::move(variants)) {}
  size_t size() const { return variants_.size(); }

  base::vector<const Type *> variants_;

private:
  mutable std::mutex mtx_;
  mutable IR::Func *repr_func_ = nullptr;
};

const Type *Var(base::vector<const Type *> variants);

} // namespace type
#endif // ICARUS_TYPE_VARIANT_H
