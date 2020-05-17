#include "ir/value/native_fn.h"

#include "gtest/gtest.h"
#include "type/function.h"
#include "type/primitive.h"

namespace {

TEST(NativeFn, Equality) {
  auto *fn_type = type::Func(core::Params<type::QualType>{}, {});

  ir::CompiledFn cf1(fn_type, {});
  ir::CompiledFn cf2(fn_type, {});

  ir::NativeFn f1(&cf1);
  ir::NativeFn f2(&cf2);

  EXPECT_EQ(f1, f1);
  EXPECT_NE(f1, f2);
}

TEST(NativeFn, Value) {
  auto *fn_type = type::Func(core::Params<type::QualType>{}, {});

  ir::CompiledFn cf(fn_type, {});

  ir::NativeFn f(&cf);
  EXPECT_EQ(f.type(), fn_type);
  EXPECT_EQ(f.get(), &cf);
}

}  // namespace
