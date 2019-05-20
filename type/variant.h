#ifndef ICARUS_TYPE_VARIANT_H
#define ICARUS_TYPE_VARIANT_H

#include <mutex>
#include "type.h"

namespace type {
Type const *Var(std::vector<Type const *> variants);

struct Variant : public Type {
  TYPE_FNS(Variant);
  Variant(std::vector<Type const *> variants)
      : variants_(std::move(variants)) {}
  size_t size() const { return variants_.size(); }

#include "visitor/type_visitors.xmacro.h"

  Type const *finalize() {
    auto *result = Var(std::move(variants_));
    ASSERT(this != result);
    delete this;
    return result;
  }

  // TODO can do better with a pair of iterators and checking if one is a subset
  // of the other.
  bool contains(type::Type const *t) const;

  std::vector<Type const *> variants_;

  mutable std::mutex mtx_;
  mutable ir::CompiledFn *repr_func_ = nullptr, *destroy_func_ = nullptr;
};

}  // namespace type
#endif  // ICARUS_TYPE_VARIANT_H
