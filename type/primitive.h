#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

#include "type/type.h"

namespace type {
enum class PrimType : char {
#define PRIMITIVE_MACRO(EnumName, name) EnumName,
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
};

struct Primitive : public Type {
 public:
  TYPE_FNS(Primitive);
  Primitive(PrimType pt) : type_(pt) {}

#include ICARUS_TYPE_VISITOR_METHODS

  bool TestEquality(void const *lhs, void const *rhs) const override;

  bool is_integral() const;

  PrimType type_;
};

}  // namespace type
#endif  // ICARUS_TYPE_PRIMITIVE_H
