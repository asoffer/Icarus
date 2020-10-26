#ifndef ICARUS_TYPE_GENERIC_STRUCT_H
#define ICARUS_TYPE_GENERIC_STRUCT_H

#include <vector>

#include "ast/scope/module.h"
#include "ast/scope/scope.h"
#include "base/any_invocable.h"
#include "core/fn_args.h"
#include "module/module.h"
#include "type/struct.h"
#include "type/type.h"

namespace type {

struct GenericStruct : LegacyType {
  explicit GenericStruct(base::any_invocable<
                         Struct const *(core::FnArgs<Typed<ir::Value>> const &)>
                             fn)
      : LegacyType(LegacyType::Flags{.is_default_initializable = 0,
                                     .is_copyable              = 1,
                                     .is_movable               = 1,
                                     .has_destructor           = 0}),
        gen_(std::move(fn)) {}
  void WriteTo(std::string *result) const override {
    result->append("generic-struct");
  }

  bool is_big() const override { return false; }

  Completeness completeness() const override {
    return Completeness::Incomplete;
  }

  Struct const *concrete(core::FnArgs<Typed<ir::Value>> const &) const;

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

 private:
  // TODO: Eventually we will want a serializable version of this.
  base::any_invocable<Struct const *(core::FnArgs<Typed<ir::Value>> const &)>
      gen_;
};

GenericStruct *GenStruct(ast::Scope const *scope,
                         std::vector<LegacyType const *> ts);

}  // namespace type

#endif  // ICARUS_TYPE_GENERIC_STRUCT_H
