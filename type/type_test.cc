#include "type/type.h"

#include "nth/test/test.h"

namespace ic::type {
namespace {

NTH_TEST("type/construction") {
  Type t = PrimitiveType(PrimitiveType::Kind::Bool);
  NTH_EXPECT(t == Bool);
  NTH_EXPECT(t.kind() == Type::Kind::Primitive);

  t = PrimitiveType(PrimitiveType::Kind::Module);
  NTH_EXPECT(t == Module);
  NTH_EXPECT(t.kind() == Type::Kind::Primitive);

  t = PrimitiveType(PrimitiveType::Kind::Type);
  NTH_EXPECT(t == Type_);
  NTH_EXPECT(t.kind() == Type::Kind::Primitive);
}


NTH_TEST("type/parameters/construction") {
  Type t0 = Parameters({});
  Type t1 = Parameters({ParametersType::Parameter{.name = 3, .type = Bool}});
  Type t2 = Parameters({ParametersType::Parameter{.name = 4, .type = Bool}});
  Type t3 = Parameters({ParametersType::Parameter{.name = 4, .type = Char}});

  NTH_EXPECT(t0 == t0);
  NTH_EXPECT(t0 != t1);
  NTH_EXPECT(t0 != t2);
  NTH_EXPECT(t0 != t3);
  NTH_EXPECT(t1 != t0);
  NTH_EXPECT(t1 == t1);
  NTH_EXPECT(t1 != t2);
  NTH_EXPECT(t1 != t3);
  NTH_EXPECT(t2 != t1);
  NTH_EXPECT(t2 != t0);
  NTH_EXPECT(t2 == t2);
  NTH_EXPECT(t2 != t3);
  NTH_EXPECT(t3 != t0);
  NTH_EXPECT(t3 != t1);
  NTH_EXPECT(t3 != t2);
  NTH_EXPECT(t3 == t3);

  NTH_EXPECT(t0.kind() == Type::Kind::Parameters);
  NTH_EXPECT(t1.kind() == Type::Kind::Parameters);
  NTH_EXPECT(t2.kind() == Type::Kind::Parameters);
  NTH_EXPECT(t3.kind() == Type::Kind::Parameters);

  NTH_EXPECT(t0 != Error);
  NTH_EXPECT(t0 != Bool);

  NTH_EXPECT(t3 ==
             Parameters({ParametersType::Parameter{.name = 4, .type = Char}}));
}

NTH_TEST("type/pointer") {
  NTH_EXPECT(Ptr(Char) == Ptr(Char));
  NTH_EXPECT(Ptr(Ptr(Char)) != Ptr(Char));
  NTH_EXPECT(Ptr(Ptr(Char)).pointee() == Ptr(Char));
  NTH_EXPECT(Ptr(Ptr(Char)).pointee().AsPointer().pointee() == Char);
}

NTH_TEST("type/buffer-pointer") {
  NTH_EXPECT(BufPtr(Char) == BufPtr(Char));
  NTH_EXPECT(BufPtr(Ptr(Char)) != Ptr(Char));
  NTH_EXPECT(BufPtr(Ptr(Char)).pointee() == Ptr(Char));
  NTH_EXPECT(BufPtr(Ptr(Char)).pointee().AsPointer().pointee() == Char);
  NTH_EXPECT(BufPtr(BufPtr(Char)) != BufPtr(Char));
  NTH_EXPECT(BufPtr(BufPtr(Char)).pointee() == BufPtr(Char));
  NTH_EXPECT(BufPtr(BufPtr(Char)).pointee().AsBufferPointer().pointee() ==
             Char);
}

NTH_TEST("type/pattern") {
  NTH_EXPECT(Pattern(Char) == Pattern(Char));
  NTH_EXPECT(Pattern(Pattern(Char)) != Pattern(Char));
  NTH_EXPECT(Pattern(Pattern(Char)).match_type() == Pattern(Char));
  NTH_EXPECT(Pattern(Pattern(Char)).match_type().AsPattern().match_type() ==
             Char);
}

NTH_TEST("type/slice") {
  NTH_EXPECT(Slice(Char) == Slice(Char));
  NTH_EXPECT(Slice(Slice(Char)) != Slice(Char));
  NTH_EXPECT(Slice(Slice(Char)).element_type() == Slice(Char));
  NTH_EXPECT(Slice(Slice(Char)).element_type().AsSlice().element_type() ==
             Char);
}

NTH_TEST("qualified-type/construction") {
  NTH_EXPECT(QualifiedType(Qualifier::Constant(), Bool).type() == Bool);
  NTH_EXPECT(QualifiedType(Qualifier::Constant(), Bool).qualifier() ==
             Qualifier::Constant());

  NTH_EXPECT(QualifiedType(Qualifier::Constant(), Bool).type() == Bool);
  NTH_EXPECT(QualifiedType(Qualifier::Unqualified(), Bool).qualifier() ==
             Qualifier::Unqualified());
}

}  // namespace
}  // namespace ic::type
