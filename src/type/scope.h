#ifndef ICARUS_TYPE_SCOPE_H
#define ICARUS_TYPE_SCOPE_H

#include "base/container/vector.h"
#include "type.h"

namespace type {
struct Scope : public Type {
  TYPE_FNS(Scope);

  explicit Scope(base::vector<const Type *> t) : types_(std::move(t)) {}
  base::vector<const Type *> types_;
};

const Scope *Scp(const base::vector<const Type *> &types);
}  // namespace type

#endif  // ICARUS_TYPE_SCOPE_H
