#include "ir/value/generic_fn.h"

#include "gtest/gtest.h"
#include "ir/value/result_buffer.h"

namespace {

TEST(GenericFn, Equality) {
  ir::GenericFn f1(
      [](compiler::WorkResources const &,
         core::Arguments<type::Typed<ir::CompleteResultRef>> const &) {
        return ir::Fn();
      });
  ir::GenericFn f2(
      [](compiler::WorkResources const &,
         core::Arguments<type::Typed<ir::CompleteResultRef>> const &) {
        return ir::Fn();
      });
  EXPECT_EQ(f1, f1);
  EXPECT_EQ(f2, f2);
  EXPECT_NE(f1, f2);
}

}  // namespace
