#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

#include "type/type.h"

struct Architecture;

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

  bool is_integral() const;

 private:
  friend struct ::Architecture;
  PrimType type_;
};

}  // namespace type
#endif  // ICARUS_TYPE_PRIMITIVE_H
