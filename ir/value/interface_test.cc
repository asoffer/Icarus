#include "ir/value/interface.h"

#include "gtest/gtest.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/type.h"

namespace ir {
namespace {

TEST(Interface, Equality) {
  EXPECT_EQ(Interface::Just(type::I64), Interface::Just(type::I64));
  EXPECT_NE(Interface::Just(type::I64), Interface::ConvertsTo(type::I64));
  EXPECT_NE(Interface::Just(type::I64), Interface::Just(type::U64));
}

TEST( Satisfiability, Just) {
  EXPECT_TRUE(Interface::Just(type::I64).SatisfiedBy(type::I64));
  EXPECT_FALSE(Interface::Just(type::I64).SatisfiedBy(type::U64));
}

TEST(Satisfiability, ConvertsTo) {
  EXPECT_TRUE(Interface::ConvertsTo(type::Ptr(type::I64))
                  .SatisfiedBy(type::Ptr(type::I64)));
  EXPECT_TRUE(Interface::ConvertsTo(type::Ptr(type::I64))
                  .SatisfiedBy(type::BufPtr(type::I64)));
  EXPECT_TRUE(
      Interface::ConvertsTo(type::Ptr(type::I64)).SatisfiedBy(type::I64));
}

}  // namespace
}  // namespace ir
