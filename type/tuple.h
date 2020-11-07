#ifndef ICARUS_TYPE_TUPLE_H
#define ICARUS_TYPE_TUPLE_H

#include "absl/types/span.h"
#include "base/lazy.h"
#include "ir/value/native_fn.h"
#include "type/type.h"

namespace type {
struct Tuple : public LegacyType {
  Tuple() = delete;
  Tuple(std::vector<Type> entries);

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  void WriteTo(std::string *result) const override;

  core::Bytes offset(size_t n, core::Arch const &arch) const;
  absl::Span<Type const> entries() const { return entries_; }
  size_t size() const { return entries_.size(); }

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  Completeness completeness() const override {
    Completeness c = Completeness::Complete;
    for (Type e : entries_) { c = std::min(c, e.get()->completeness()); }
    return c;
  }

  std::vector<Type> entries_;

  base::lazy<ir::NativeFn> destroy_func_;
  base::lazy<ir::NativeFn> init_func_;
  base::lazy<ir::NativeFn> copy_assign_func_;
  base::lazy<ir::NativeFn> move_assign_func_;
};

Type Tup(std::vector<Type> entries);

Type Void();

}  // namespace type

#endif  // ICARUS_TYPE_TUPLE_H
