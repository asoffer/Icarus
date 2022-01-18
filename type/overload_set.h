#ifndef ICARUS_TYPE_OVERLOAD_SET_H
#define ICARUS_TYPE_OVERLOAD_SET_H

#include <vector>

#include "absl/container/flat_hash_set.h"
#include "type/type.h"

namespace type {

struct OverloadSet : LegacyType {
  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  absl::flat_hash_set<Type> const &members() const { return members_; }

  Completeness completeness() const override { return Completeness::Complete; }

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  // Not considered big because it only makes sense to pass around at
  // compile-time anyway.
  bool is_big() const override { return false; }

 private:
  friend OverloadSet const *MakeOverloadSet(
      absl::flat_hash_set<Type> const &ts);

  OverloadSet(absl::flat_hash_set<Type> cs)
      : LegacyType(IndexOf<OverloadSet>(),
                   LegacyType::Flags{.is_default_initializable = 0,
                                     .is_copyable              = 1,
                                     .is_movable               = 1,
                                     .has_destructor           = 0}),
        members_(std::move(cs)) {}

  absl::flat_hash_set<Type> members_;
};

OverloadSet const *MakeOverloadSet(absl::flat_hash_set<Type> const &cs);

}  // namespace type

#endif  // ICARUS_TYPE_OVERLOAD_SET_H
