#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

#include <mutex>
#include "type/type.h"

struct Architecture;

namespace type {
enum class PrimType : char {
#define PRIMITIVE_MACRO(GlobalName, EnumName, name) EnumName,
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
};

struct Primitive : public Type {
public:
  TYPE_FNS(Primitive);
  Primitive(PrimType pt) : type_(pt) {}

private:
  friend struct ::Architecture;
  PrimType type_;

  mutable std::mutex mtx_;
  mutable IR::Func *repr_func_ = nullptr;
};

} // namespace type
#endif // ICARUS_TYPE_PRIMITIVE_H
