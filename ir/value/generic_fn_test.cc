#include "ir/value/generic_fn.h"

#include "gtest/gtest.h"
#include "ir/value/value.h"

namespace {

TEST(GenericFn, Equality) {
  ir::GenericFn f1([](core::Arguments<type::Typed<ir::Value>> const&) {
    return ir::NativeFn(nullptr);
  });
  ir::GenericFn f2([](core::Arguments<type::Typed<ir::Value>> const&) {
    return ir::NativeFn(nullptr);
  });
  EXPECT_EQ(f1, f1);
  EXPECT_EQ(f2, f2);
  EXPECT_NE(f1, f2);
}

}  // namespace
