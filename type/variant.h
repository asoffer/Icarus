#ifndef ICARUS_TYPE_VARIANT_H
#define ICARUS_TYPE_VARIANT_H

#include <mutex>
#include <vector>

#include "absl/types/span.h"
#include "core/arch.h"
#include "ir/value/native_fn.h"
#include "type.h"

namespace ir {
struct CompiledFn;
}  // namespace ir

namespace type {
Type const *Var(std::vector<Type const *> variants);

std::vector<Type const *> MultiVar(
    absl::Span<absl::Span<Type const *const> const> variants);

struct Variant : public Type {
  TYPE_FNS(Variant);
  Variant(std::vector<Type const *> variants)
      : variants_(std::move(variants)) {}
  size_t size() const { return variants_.size(); }

  bool IsDefaultInitializable() const { return false; }
  bool IsCopyable() const;
  bool IsMovable() const;
  bool HasDestructor() const;

  // Alignment of the alternatives present in the variant, excluding the type
  // tag. For example, if a type requires 8-byte alignment, then the alignment
  // of `bool | int16` will be 8, but the alternative alignment will be 2. The
  // name is intended to be read as "alignment of the alternatives in the
  // variant", rather than as "a different kind of alignment."
  core::Alignment alternative_alignment(core::Arch const &arch) const;

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  // TODO can do better with a pair of iterators and checking if one is a subset
  // of the other.
  bool contains(type::Type const *t) const;

  std::vector<Type const *> variants_;

  mutable std::mutex mtx_;
  mutable std::optional<ir::NativeFn> destroy_func_;
};

}  // namespace type
#endif  // ICARUS_TYPE_VARIANT_H
