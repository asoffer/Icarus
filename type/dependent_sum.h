#ifndef ICARUS_TYPE_DEPENDENT_SUM_H
#define ICARUS_TYPE_DEPENDENT_SUM_H

#include "type/basic.h"
#include "type/family.h"

namespace ic::type {

struct DependentSumType : internal_type::BasicType {
  Family family() const;
  Type index_type() const { return family().index_type(); }

  friend void NthPrint(auto& p, auto& fmt, DependentSumType d) {
    p.write("Sum(family.");
    fmt(p, d.data());
    p.write(")");
  }

 private:
  friend Type;
  friend DependentSumType DependentSum(Family family);

  explicit constexpr DependentSumType() = default;
  explicit constexpr DependentSumType(uint64_t n)
      : BasicType(Type::Kind::DependentSum, n) {}
};

DependentSumType DependentSum(Family family);

}  // namespace ic::type

#endif  // ICARUS_TYPE_DEPENDENT_SUM_H
