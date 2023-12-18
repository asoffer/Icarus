#ifndef ICARUS_TYPE_FAMILY_H
#define ICARUS_TYPE_FAMILY_H

#include <utility>

#include "type/basic.h"

namespace ic::type {

struct Family {
  explicit constexpr Family(Type index_type, void const* evaluator)
      : index_(index_type), evaluator_(evaluator) {}

  friend constexpr bool operator==(Family, Family) = default;

  template <typename H>
  friend H AbslHashValue(H h, Family f) {
    return H::combine(std::move(h), f.index_, f.evaluator_);
  }

  Type index_type() const { return index_; }
  void const* evaluator() const { return evaluator_; }

 private:
  Type index_;
  void const* evaluator_;
};

}  // namespace ic::type

#endif // ICARUS_TYPE_FAMILY_H
