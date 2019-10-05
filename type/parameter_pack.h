#ifndef ICARUS_TYPE_PARAMETER_PACK_H
#define ICARUS_TYPE_PARAMETER_PACK_H

#include "type/type.h"

namespace type {

struct ParameterPack : public Type {
  TYPE_FNS(ParameterPack);
  ParameterPack(Type const *t) : elem(t) {}

#include ICARUS_TYPE_VISITOR_METHODS

  Type const *elem;
};

ParameterPack const *Pack(Type const *t);

}  // namespace type

#endif  // ICARUS_TYPE_PARAMETER_PACK_H
