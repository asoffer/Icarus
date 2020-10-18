#ifndef ICARUS_TYPE_PARAMETER_PACK_H
#define ICARUS_TYPE_PARAMETER_PACK_H

#include "type/type.h"

namespace type {

struct ParameterPack : public LegacyType {
  TYPE_FNS(ParameterPack);
  ParameterPack(LegacyType const *t) : LegacyType(t->flags()), elem(t) {}

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  // TODO: Does this make any sense?
  Completeness completeness() const override { return Completeness::Complete; }

  LegacyType const *elem;
};

ParameterPack const *Pack(LegacyType const *t);

}  // namespace type

#endif  // ICARUS_TYPE_PARAMETER_PACK_H
