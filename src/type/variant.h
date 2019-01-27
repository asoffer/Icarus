#ifndef ICARUS_TYPE_VARIANT_H
#define ICARUS_TYPE_VARIANT_H

#include <mutex>
#include "type.h"

namespace type {
Type const *Var(base::vector<Type const *> variants);

struct Variant : public Type {
  TYPE_FNS(Variant);
  Variant(base::vector<Type const *> variants)
      : variants_(std::move(variants)) {}
  size_t size() const { return variants_.size(); }

  Type const *finalize() {
    auto *result = Var(std::move(variants_));
    ASSERT(this != result);
    delete this;
    return result;
  }

  void EmitDestroy(ir::Register reg, Context *ctx) const override;

  bool needs_destroy() const override;

  // TODO can do better with a pair of iterators and checking if one is a subset
  // of the other.
  bool contains(type::Type const *t) const;

  bool IsDefaultInitializable() const override { return false; }
  bool IsCopyable() const;
  base::vector<Type const *> variants_;

 private:
  mutable std::mutex mtx_;
  mutable ir::Func *repr_func_ = nullptr, *destroy_func_ = nullptr;
};

}  // namespace type
#endif  // ICARUS_TYPE_VARIANT_H
