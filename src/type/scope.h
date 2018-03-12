#ifndef ICARUS_TYPE_SCOPE_H
#define ICARUS_TYPE_SCOPE_H

#include "type.h"

namespace type {
struct Scope : public Type {
  TYPE_FNS(Scope);

  explicit Scope(const Type *t) : type_(t) {}
  const Type *type_;
};

const Scope *Scp(const Type *t);
} // namespace type

#endif // ICARUS_TYPE_SCOPE_H
