#ifndef ICARUS_TYPE_GENERIC_STRUCT_H
#define ICARUS_TYPE_GENERIC_STRUCT_H

#include "base/container/vector.h"
#include "type/callable.h"
#include "type/type.h"

struct Context;

namespace type {
struct GenericStruct : public Callable {
  TYPE_FNS(GenericStruct);
  GenericStruct(Type const *t) : deps_({t}) {}
  GenericStruct(base::vector<Type const *> ts) : deps_(std::move(ts)) {}

  base::vector<Type const*> deps_;
};

GenericStruct *GenStruct(Type const *t);
GenericStruct *GenStruct(base::vector<Type const *> ts);
}  // namespace type

#endif  // ICARUS_TYPE_GENERIC_STRUCT_H
