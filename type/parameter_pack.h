#ifndef ICARUS_TYPE_PARAMETER_PACK_H
#define ICARUS_TYPE_PARAMETER_PACK_H

#include "type/type.h"

namespace type {

struct ParameterPack : public Type {
  TYPE_FNS(ParameterPack);
  ParameterPack(Type const *t) : elem(t) {}

#include "visitor/type_visitors.xmacro.h"

  Type const *elem;
};

ParameterPack const *Pack(Type const *t);

}  // namespace type

#endif  // ICARUS_TYPE_PARAMETER_PACK_H
