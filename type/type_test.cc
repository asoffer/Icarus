#include "type/type.h"

#include "nth/test/test.h"

namespace ic::type {
namespace {

NTH_TEST("type/construction") {
  Type t = PrimitiveType(PrimitiveType::Kind::Bool);
  NTH_EXPECT(t == Bool);
  NTH_EXPECT(t.kind() == Type::Kind::Primitive);
}

}  // namespace
}  // namespace ic::type
