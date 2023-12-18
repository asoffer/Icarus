#ifndef ICARUS_TYPE_DEPENDENT_PRODUCT_H
#define ICARUS_TYPE_DEPENDENT_PRODUCT_H

#include "type/basic.h"
#include "type/family.h"

namespace ic::type {

struct DependentProductType : internal_type::BasicType {
  Family family() const;
  Type index_type() const { return family().index_type(); }

  friend void NthPrint(auto& p, auto& fmt, DependentProductType d) {
    p.write("Product(family.");
    fmt(p, d.data());
    p.write(")");
  }

 private:
  friend Type;
  friend DependentProductType DependentProduct(Family family);

  explicit constexpr DependentProductType() = default;
  explicit constexpr DependentProductType(uint64_t n)
      : BasicType(Type::Kind::DependentProduct, n) {}
};

DependentProductType DependentProduct(Family family);

}  // namespace ic::type

#endif  // ICARUS_TYPE_DEPENDENT_PRODUCT_H
