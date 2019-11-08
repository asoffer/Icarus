#ifndef ICARUS_TYPE_PARAMETER_PACK_H
#define ICARUS_TYPE_PARAMETER_PACK_H

#include "type/type.h"

namespace type {

struct ParameterPack : public Type {
  TYPE_FNS(ParameterPack);
  ParameterPack(Type const *t) : elem(t) {}

  void ExtractDefiningModules(absl::flat_hash_set<module::BasicModule const *>
                                  *modules) const override {
    return module::ExtractDefiningModules::Extract(this, modules);
  }

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  Type const *elem;
};

ParameterPack const *Pack(Type const *t);

}  // namespace type

#endif  // ICARUS_TYPE_PARAMETER_PACK_H
