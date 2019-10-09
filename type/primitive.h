#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

#include "type/basic_type.h"
#include "type/type.h"

namespace type {

struct Primitive : public Type {
 public:
  TYPE_FNS(Primitive);
  Primitive(BasicType pt) : type_(pt) {}

#include ICARUS_TYPE_VISITOR_METHODS

  bool TestEquality(void const *lhs, void const *rhs) const override;

  bool is_integral() const;

  BasicType type_;
};

}  // namespace type
#endif  // ICARUS_TYPE_PRIMITIVE_H
