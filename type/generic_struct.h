#ifndef ICARUS_TYPE_GENERIC_STRUCT_H
#define ICARUS_TYPE_GENERIC_STRUCT_H

#include <vector>

#include "ast/scope/module.h"
#include "ast/scope/scope.h"
#include "base/any_invocable.h"
#include "core/arguments.h"
#include "module/module.h"
#include "type/callable.h"
#include "type/primitive.h"
#include "type/struct.h"
#include "type/type.h"

namespace type {

struct GenericStruct : Callable {
  explicit GenericStruct(
      base::any_invocable<std::pair<core::Params<QualType>, Struct const *>(
          core::Arguments<Typed<ir::Value>> const &)>
          fn)
      : gen_(std::move(fn)) {}
  void WriteTo(std::string *result) const override {
    result->append("generic-struct");
  }

  bool is_big() const override { return false; }

  Completeness completeness() const override {
    return Completeness::Incomplete;
  }

  std::vector<type::Type> return_types(
      core::Arguments<type::Typed<ir::Value>> const &args) const override {
    return {Type(Type_)};
  }

  auto Instantiate(core::Arguments<Typed<ir::Value>> const &args) const {
    return gen_(args);
  }

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

 private:
  // TODO: Eventually we will want a serializable version of this.
  base::any_invocable<std::pair<core::Params<QualType>, Struct const *>(
      core::Arguments<Typed<ir::Value>> const &)>
      gen_;
};

}  // namespace type

#endif  // ICARUS_TYPE_GENERIC_STRUCT_H
