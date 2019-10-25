#ifndef ICARUS_TYPE_GENERIC_STRUCT_H
#define ICARUS_TYPE_GENERIC_STRUCT_H

#include <vector>

#include "ast/scope/scope.h"
#include "ast/scope/module.h"
#include "module/module.h"
#include "type/callable.h"
#include "type/type.h"

namespace type {
struct GenericStruct : public Callable {
  TYPE_FNS(GenericStruct);
  GenericStruct(ast::Scope const *scope, std::vector<Type const *> ts)
      : scope_(scope),
        mod_(scope->Containing<ast::ModuleScope>()->module()),
        deps_(std::move(ts)) {}

#include ICARUS_TYPE_VISITOR_METHODS

  module::BasicModule const *defining_module() const { return mod_; }

  ast::Scope const *scope_       = nullptr;
  module::BasicModule const *mod_ = nullptr;
  std::vector<Type const *> deps_;
};

GenericStruct *GenStruct(ast::Scope const *scope,
                         std::vector<Type const *> ts);
}  // namespace type

#endif  // ICARUS_TYPE_GENERIC_STRUCT_H
