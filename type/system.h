#ifndef ICARUS_TYPE_SYSTEM_H
#define ICARUS_TYPE_SYSTEM_H

#include "base/flyweight_set.h"
#include "base/iterator.h"
#include "base/log.h"
#include "type/type.h"

namespace type {

struct TypeSystem {
  auto size() const { return types_.size(); }

  auto types() const {
    return base::iterator_range(types_.begin(), types_.end());
  }

  std::pair<size_t, bool> insert(Type t) {
    auto [iter, inserted] = types_.insert(t);
    return std::pair<size_t, bool>(types_.index(iter), inserted);
  }

  size_t index(Type t) const { return types_.index(t); }
  size_t end_index() const { return types_.end_index(); }

  Type from_index(size_t n) const { return types_.from_index(n); }

 private:
  base::flyweight_set<Type> types_;
};

// TODO: Long-term we do not want to have a global, but it's useful as a
// migration step.
inline TypeSystem GlobalTypeSystem;

}  // namespace type

#endif  // ICARUS_TYPE_SYSTEM_H
