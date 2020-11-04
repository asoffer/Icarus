#ifndef ICARUS_TYPE_OVERLOAD_SET_H
#define ICARUS_TYPE_OVERLOAD_SET_H

#include <vector>

#include "absl/container/flat_hash_set.h"
#include "type/callable.h"
#include "type/type.h"

namespace type {

Callable const *MakeOverloadSet(
    absl::flat_hash_set<Callable const *> const &cs);
Callable const *MakeOverloadSet(absl::flat_hash_set<Callable const *> &&cs);

struct OverloadSet : Callable {
  TYPE_FNS(OverloadSet);

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  absl::flat_hash_set<type::Callable const *> const &members() const {
    return callables_;
  }

  Completeness completeness() const override { return Completeness::Complete; }

  std::vector<type::Type> return_types(
      core::Arguments<type::Typed<ir::Value>> const &args) const override;

  // Not considered big because it only makes sense to pass around at
  // compile-time anyway.
  bool is_big() const override { return false; }

 private:
  friend Callable const *MakeOverloadSet(
      absl::flat_hash_set<Callable const *> const &cs);
  friend Callable const *MakeOverloadSet(
      absl::flat_hash_set<Callable const *> &&cs);

  OverloadSet(absl::flat_hash_set<type::Callable const *> cs)
      : callables_(std::move(cs)) {}

  absl::flat_hash_set<type::Callable const *> callables_;
};

}  // namespace type

#endif  // ICARUS_TYPE_OVERLOAD_SET_H
