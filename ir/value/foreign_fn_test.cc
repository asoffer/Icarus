#include "ir/value/foreign_fn.h"

#include "gtest/gtest.h"
#include "type/function.h"
#include "type/primitive.h"

namespace {

void TestFn1() {}
void TestFn2() {}

TEST(ForeignFn, Equality) {
  auto *fn_type = type::Func(core::Params<type::QualType>{core::AnonymousParam(
                                 type::QualType::NonConstant(type::I64))},
                             {});

  ir::ForeignFn f1(TestFn1, fn_type);
  ir::ForeignFn f2(TestFn1, fn_type);

  ir::ForeignFn f3(
      TestFn1, type::Func(core::Params<type::QualType>{core::Param(
                              "abc", type::QualType::NonConstant(type::I64))},
                          {}));

  ir::ForeignFn f4(TestFn2, fn_type);

  EXPECT_NE(f1, f3);
  EXPECT_NE(f1, f4);
}

TEST(ForeignFn, Value) {
  auto *fn_type = type::Func(core::Params<type::QualType>{core::AnonymousParam(
                                 type::QualType::NonConstant(type::I64))},
                             {});

  ir::ForeignFn f(TestFn1, fn_type);
  EXPECT_EQ(f.type(), fn_type);
  EXPECT_EQ(f.get(), &TestFn1);
}

}  // namespace
