#include "ir/value/native_fn.h"

#include "gtest/gtest.h"
#include "type/function.h"
#include "type/primitive.h"

namespace {

TEST(NativeFn, CastToBool) {
  ir::NativeFn f;
  EXPECT_FALSE(f);

  auto *fn_type = type::Func(core::Params<type::QualType>{}, {});
  ir::CompiledFn cf(fn_type, {});
  ir::NativeFn::Data d{
      .fn   = &cf,
      .type = fn_type,
  };
  EXPECT_TRUE(ir::NativeFn(&d));
}

TEST(NativeFn, Equality) {
  auto *fn_type = type::Func(core::Params<type::QualType>{}, {});

  ir::CompiledFn cf1(fn_type, {});
  ir::CompiledFn cf2(fn_type, {});

  ir::NativeFn::Data d1{
      .fn   = &cf1,
      .type = fn_type,
  };
  ir::NativeFn::Data d2{
      .fn   = &cf2,
      .type = fn_type,
  };

  ir::NativeFn f1(&d1);
  ir::NativeFn f2(&d2);

  EXPECT_EQ(f1, f1);
  EXPECT_NE(f1, f2);
}

TEST(NativeFn, Value) {
  auto *fn_type = type::Func(core::Params<type::QualType>{}, {});

  ir::CompiledFn cf(fn_type, {});
  ir::NativeFn::Data d{
      .fn   = &cf,
      .type = fn_type,
  };
  ir::NativeFn f(&d);
  EXPECT_EQ(f.type(), fn_type);
  EXPECT_EQ(&*f, &cf);
}

}  // namespace
