#include "ir/value/native_fn.h"

#include "gtest/gtest.h"
#include "type/function.h"
#include "type/primitive.h"

namespace {

TEST(NativeFn, CastToBool) {
  ir::NativeFn f;
  EXPECT_FALSE(f);

  auto *fn_type = type::Func(core::Parameters<type::QualType>{}, {});
  ir::NativeFunctionInformation info{.fn = ir::Subroutine(fn_type)};
  EXPECT_TRUE(ir::NativeFn(&info));
}

TEST(NativeFn, Equality) {
  auto *fn_type = type::Func(core::Parameters<type::QualType>{}, {});

  ir::NativeFunctionInformation info1{.fn = ir::Subroutine(fn_type)};
  ir::NativeFunctionInformation info2{.fn = ir::Subroutine(fn_type)};

  ir::NativeFn f1(&info1);
  ir::NativeFn f2(&info2);

  EXPECT_EQ(f1, f1);
  EXPECT_NE(f1, f2);
}

TEST(NativeFn, Value) {
  auto *fn_type = type::Func(core::Parameters<type::QualType>{}, {});

  ir::NativeFunctionInformation info{.fn = ir::Subroutine(fn_type)};
  ir::NativeFn f(&info);
  EXPECT_EQ(f.type(), fn_type);
}

}  // namespace
