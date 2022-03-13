#include "ir/value/reg_or.h"

#include "base/universal_print.h"
#include "gtest/gtest.h"

namespace ir {
namespace {

TEST(RegOr, ImplicitConstruction) {
  auto id = [](RegOr<int> r) { return r; };

  EXPECT_FALSE(id(3).is_reg());
  EXPECT_EQ(id(3).value(), 3);

  EXPECT_TRUE(id(Reg::Parameter(3)).is_reg());
  EXPECT_EQ(id(Reg::Parameter(3)).reg(), Reg::Parameter(3));
}

TEST(RegOr, Resolve) {
  auto f = [](Reg r) { return 4; };
  EXPECT_EQ(RegOr<int>(Reg::Output(1)).resolve(f), 4);
  EXPECT_EQ(RegOr<int>(2).resolve(f), 2);
}

TEST(RegOr, Apply) {
  auto f = [](auto x) {
    if constexpr (std::is_same_v<std::decay_t<decltype(x)>, Reg>) {
      return x.template as<Reg::Kind::Value>();
    } else {
      return x * x;
    }
  };
  EXPECT_EQ(RegOr<int>(Reg(1)).apply(f), 1);
  EXPECT_EQ(RegOr<int>(2).apply(f), 4);
}

TEST(RegOr, UniversalPrintToString) {
  EXPECT_EQ(base::UniversalPrintToString(RegOr<int>(3)), "3");
  EXPECT_EQ(base::UniversalPrintToString(RegOr<bool>(true)), "true");
  EXPECT_EQ(base::UniversalPrintToString(RegOr<bool>(Reg(4))),
            base::UniversalPrintToString(Reg(4)));
}

TEST(RegOr, Comparison) {
  EXPECT_EQ(RegOr<int>(17), RegOr<int>(17));
  EXPECT_FALSE(RegOr<int>(17) != RegOr<int>(17));

  EXPECT_FALSE(RegOr<int>(Reg(17)) == RegOr<int>(17));
  EXPECT_NE(RegOr<int>(Reg(17)), RegOr<int>(17));
}

}  // namespace
}  // namespace ir
