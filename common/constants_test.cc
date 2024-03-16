#include "common/constants.h"

#include "nth/test/test.h"

namespace ic {
namespace {

NTH_TEST("constants/types/primitives") {
  NTH_EXPECT(type::Bool.kind() == type::Type::Kind::Primitive);
  NTH_EXPECT(type::Char.kind() == type::Type::Kind::Primitive);
  NTH_EXPECT(type::I32.kind() == type::Type::Kind::Primitive);
  NTH_EXPECT(type::Module.kind() == type::Type::Kind::Primitive);
  NTH_EXPECT(type::Type_.kind() == type::Type::Kind::Primitive);

  NTH_EXPECT(type::Bool.primitive_kind() == type::PrimitiveType::Kind::Bool);
  NTH_EXPECT(type::Char.primitive_kind() == type::PrimitiveType::Kind::Char);
  NTH_EXPECT(type::I32.primitive_kind() == type::PrimitiveType::Kind::I32);
  NTH_EXPECT(type::Module.primitive_kind() ==
             type::PrimitiveType::Kind::Module);
  NTH_EXPECT(type::Type_.primitive_kind() == type::PrimitiveType::Kind::Type);
}


NTH_TEST("constants/types/slices") {
  auto ssc = type::Slice(type::Slice(type::Char));
  auto sc  = type::Slice(type::Char);
  NTH_EXPECT(sc.element_type() == type::Char);
  NTH_EXPECT(ssc.element_type() == sc);
  NTH_EXPECT(ssc.element_type().index() == sc.index());
}

NTH_TEST("constants/types/pointers") {
  auto ppc = type::Ptr(type::Ptr(type::Char));
  auto pc  = type::Ptr(type::Char);
  NTH_EXPECT(pc.pointee() == type::Char);
  NTH_EXPECT(ppc.pointee() == pc);
  NTH_EXPECT(ppc.pointee().index() == pc.index());
}

NTH_TEST("constants/types/buffer-pointers") {
  auto ppc = type::BufPtr(type::BufPtr(type::Char));
  auto pc  = type::BufPtr(type::Char);
  NTH_EXPECT(pc.pointee() == type::Char);
  NTH_EXPECT(ppc.pointee() == pc);
  NTH_EXPECT(ppc.pointee().index() == pc.index());
}

}  // namespace
}  // namespace ic
