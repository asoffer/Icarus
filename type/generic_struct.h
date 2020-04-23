#ifndef ICARUS_TYPE_GENERIC_STRUCT_H
#define ICARUS_TYPE_GENERIC_STRUCT_H

#include <vector>

#include "ast/scope/module.h"
#include "ast/scope/scope.h"
#include "module/module.h"
#include "type/type.h"

namespace type {
struct GenericStruct : public Type {
  TYPE_FNS(GenericStruct);
  GenericStruct(ast::Scope const *scope, std::vector<Type const *> ts)
      : Type(Type::Flags{.is_default_initializable = 0,
                         .is_copyable              = 1,
                         .is_movable               = 1,
                         .has_destructor           = 0}),
        scope_(scope),
        mod_(nullptr /* TODO */),
        deps_(std::move(ts)) {}

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  module::BasicModule const *defining_module() const { return mod_; }

  ast::Scope const *scope_        = nullptr;
  module::BasicModule const *mod_ = nullptr;
  std::vector<Type const *> deps_;
};

GenericStruct *GenStruct(ast::Scope const *scope, std::vector<Type const *> ts);
}  // namespace type

#endif  // ICARUS_TYPE_GENERIC_STRUCT_H
