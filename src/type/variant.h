#ifndef ICARUS_TYPE_VARIANT_H
#define ICARUS_TYPE_VARIANT_H

#include "type.h"

namespace type {
struct Variant : public Type {
  TYPE_FNS(Variant);
  Variant(std::vector<const Type *> variants)
      : variants_(std::move(variants)) {}
  size_t size() const { return variants_.size(); }

  std::vector<const Type *> variants_;

private:
  mutable IR::Func *repr_func_ = nullptr;
};

const Type *Var(std::vector<const Type *> variants);

} // namespace type
#endif // ICARUS_TYPE_VARIANT_H
