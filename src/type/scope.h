#ifndef ICARUS_TYPE_SCOPE_H
#define ICARUS_TYPE_SCOPE_H

#include <vector>
#include "type.h"

namespace type {
struct Scope : public Type {
  TYPE_FNS(Scope);

  explicit Scope(std::vector<const Type *> t) : types_(std::move(t)) {}
  std::vector<const Type *> types_;
};

const Scope *Scp(const std::vector<const Type *> &types);
} // namespace type

#endif // ICARUS_TYPE_SCOPE_H
