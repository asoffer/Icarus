#include "ir/value/reg.h"

#include "base/universal_print.h"
#include "gtest/gtest.h"

namespace ir {
namespace {

TEST(Reg, ParameterConstruction) {
  auto r = Reg::Parameter(17);
  EXPECT_TRUE(r.is<Reg::Kind::Parameter>());
  EXPECT_FALSE(r.is<Reg::Kind::Output>());
  EXPECT_EQ(r.as<Reg::Kind::Parameter>(), 17);
}

TEST(Reg, OutputConstruction) {
  auto r = Reg::Output(17);
  EXPECT_TRUE(r.is<Reg::Kind::Output>());
  EXPECT_FALSE(r.is<Reg::Kind::Parameter>());
  EXPECT_EQ(r.as<Reg::Kind::Output>(), 17);
}

TEST(Reg, UniversalPrintToString) {
  EXPECT_EQ(base::UniversalPrintToString(Reg(17)), "r.17");
  EXPECT_EQ(base::UniversalPrintToString(Reg::Parameter(17)), "param.17");
  EXPECT_EQ(base::UniversalPrintToString(Reg::Output(17)), "out.17");
  EXPECT_EQ(base::UniversalPrintToString(Reg::StackAllocation(17)), "alloc.17");
}

TEST(Reg, Compare) {
  EXPECT_EQ(Reg(17), Reg(17));
  EXPECT_FALSE(Reg(17) != Reg(17));

  EXPECT_NE(Reg(1), Reg(17));
  EXPECT_FALSE(Reg(1) == Reg(17));

  EXPECT_NE(Reg(17), Reg::Parameter(17));
  EXPECT_NE(Reg(17), Reg::Output(17));
  EXPECT_NE(Reg::Parameter(17), Reg::Output(17));

  EXPECT_EQ(Reg::Parameter(17), Reg::Parameter(17));
  EXPECT_EQ(Reg::Output(17), Reg::Output(17));
}

TEST(Reg, Kind) {
  EXPECT_EQ(Reg(1).kind(), Reg::Kind::Value);
  EXPECT_EQ(Reg::Parameter(1).kind(), Reg::Kind::Parameter);
  EXPECT_EQ(Reg::Output(1).kind(), Reg::Kind::Output);
}

TEST(Reg, RawValue) {
  EXPECT_EQ(Reg(1).raw_value(), 1);
  EXPECT_EQ(Reg::Parameter(12).raw_value(), 12);
  EXPECT_EQ(Reg::Output(123).raw_value(), 123);
}

}  // namespace
}  // namespace ir
