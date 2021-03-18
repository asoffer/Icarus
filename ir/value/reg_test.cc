#include "ir/value/reg.h"

#include "base/stringify.h"
#include "gtest/gtest.h"

namespace ir {
namespace {

TEST(Reg, ArgConstruction) {
  auto r = Reg::Arg(17);
  EXPECT_TRUE(r.is_arg());
  EXPECT_FALSE(r.is_out());
  EXPECT_EQ(r.arg_value(), 17);
}

TEST(Reg, OutConstruction) {
  auto r = Reg::Out(17);
  EXPECT_TRUE(r.is_out());
  EXPECT_FALSE(r.is_arg());
  EXPECT_EQ(r.out_value(), 17);
}

TEST(Reg, Stringify) {
  using base::stringify;
  EXPECT_EQ(stringify(Reg(17)), "r.17");
  EXPECT_EQ(stringify(Reg::Arg(17)), "arg.17");
  EXPECT_EQ(stringify(Reg::Out(17)), "out.17");
}

TEST(Reg, Compare) {
  EXPECT_EQ(Reg(17), Reg(17));
  EXPECT_FALSE(Reg(17) != Reg(17));

  EXPECT_NE(Reg(1), Reg(17));
  EXPECT_FALSE(Reg(1) == Reg(17));

  EXPECT_NE(Reg(17), Reg::Arg(17));
  EXPECT_NE(Reg(17), Reg::Out(17));
  EXPECT_NE(Reg::Arg(17), Reg::Out(17));

  EXPECT_EQ(Reg::Arg(17), Reg::Arg(17));
  EXPECT_EQ(Reg::Out(17), Reg::Out(17));
}

TEST(Reg, Kind) {
  EXPECT_EQ(Reg(1).kind(), Reg::Kind::Value);
  EXPECT_EQ(Reg::Arg(1).kind(), Reg::Kind::Argument);
  EXPECT_EQ(Reg::Out(1).kind(), Reg::Kind::Output);
}

}  // namespace
}  // namespace ir
