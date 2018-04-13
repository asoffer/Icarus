#include "test.h"

#include "frontend/numbers.h"

using test::Holds;

TEST(Base2Integer) {
  EXPECT(ParseNumber("0b0"), Holds(0));
  EXPECT(ParseNumber("0b1"), Holds(1));
  EXPECT(ParseNumber("0b01"), Holds(1));
  EXPECT(ParseNumber("0b0__1"), Holds(1));
  EXPECT(ParseNumber("0b10"), Holds(2));
  EXPECT(ParseNumber("0b10_"), Holds(2));
  EXPECT(ParseNumber("0b__10"), Holds(2));
  EXPECT(ParseNumber("0b010"), Holds(2));
  EXPECT(ParseNumber("0b01____________________________________0"), Holds(2));
  EXPECT(ParseNumber("0b00000000000000000000000000000000"),
         Holds<std::string>());
  EXPECT(ParseNumber("0b1111111111111111111111111111111"),
         Holds(std::numeric_limits<i32>::max()));
  EXPECT(ParseNumber("0b111_1111_1111_1111_1111_1111_1111_1111"),
         Holds(std::numeric_limits<i32>::max()));
  EXPECT(ParseNumber("0b10000000000000000000000000000000"),
         Holds<std::string>());
  EXPECT(ParseNumber("0b"), Holds<std::string>());
  EXPECT(ParseNumber("0b_"), Holds<std::string>());
}

TEST(Base8Integer) {
  EXPECT(ParseNumber("0o0"), Holds(0));
  EXPECT(ParseNumber("0o1"), Holds(1));
  EXPECT(ParseNumber("0o07"), Holds(7));
  EXPECT(ParseNumber("0o0_7"), Holds(7));
  EXPECT(ParseNumber("0o01"), Holds(1));
  EXPECT(ParseNumber("0o11"), Holds(9));
  EXPECT(ParseNumber("0o_11"), Holds(9));
  EXPECT(ParseNumber("0o11__"), Holds(9));
  EXPECT(ParseNumber("0o17777777777"),
         Holds(std::numeric_limits<i32>::max()));
  EXPECT(ParseNumber("0o177______________77777_____________777"),
         Holds(std::numeric_limits<i32>::max()));
  EXPECT(ParseNumber("0o20000000000"), Holds<std::string>());
  EXPECT(ParseNumber("0o7777777777777777"), Holds<std::string>());
  EXPECT(ParseNumber("0o77_77_77_77_77_77_77_77"), Holds<std::string>());
  EXPECT(ParseNumber("0o"), Holds<std::string>());
  EXPECT(ParseNumber("0o_"), Holds<std::string>());
}

TEST(Base10Integer) {
  EXPECT(ParseNumber("0d0"), Holds(0));
  EXPECT(ParseNumber("0d07"), Holds(7));
  EXPECT(ParseNumber("0d01"), Holds(1));
  EXPECT(ParseNumber("0d11"), Holds(11));
  EXPECT(ParseNumber("0d2147483647"), Holds(std::numeric_limits<i32>::max()));
  EXPECT(ParseNumber("0d2147483648"), Holds<std::string>());
  EXPECT(ParseNumber("0d9999999999"), Holds<std::string>());
  EXPECT(ParseNumber("0"), Holds(0));
  EXPECT(ParseNumber("7"), Holds(7));
  EXPECT(ParseNumber("1"), Holds(1));
  EXPECT(ParseNumber("11"), Holds(11));
  EXPECT(ParseNumber("2147483647"), Holds(std::numeric_limits<i32>::max()));
  EXPECT(ParseNumber("2147483648"), Holds<std::string>());
  EXPECT(ParseNumber("9999999999"), Holds<std::string>());
  EXPECT(ParseNumber("0d"), Holds<std::string>());
  EXPECT(ParseNumber("0d_"), Holds<std::string>());
}

TEST(Base16Integer) {
  EXPECT(ParseNumber("0x0"), Holds(0));
  EXPECT(ParseNumber("0x07"), Holds(7));
  EXPECT(ParseNumber("0x01"), Holds(1));
  EXPECT(ParseNumber("0x11"), Holds(17));
  EXPECT(ParseNumber("0x7fffffff"), Holds(std::numeric_limits<i32>::max()));
  EXPECT(ParseNumber("0x80000000"), Holds<std::string>());
  EXPECT(ParseNumber("0xffffffff"), Holds<std::string>());
  EXPECT(ParseNumber("0x"), Holds<std::string>());
  EXPECT(ParseNumber("0x_"), Holds<std::string>());
}

TEST(Base2Real) {
  EXPECT(ParseNumber("0b0.0"), Holds(0.0));
  EXPECT(ParseNumber("0b.0"), Holds(0.0));
  EXPECT(ParseNumber("0b1."), Holds(1.0));
  EXPECT(ParseNumber("0b1.0"), Holds(1.0));
  EXPECT(ParseNumber("0b.1"), Holds(0.5));
  EXPECT(ParseNumber("0b.001"), Holds(0.125));
  EXPECT(ParseNumber("0b_1__0_.1_"), Holds(2.5));
  EXPECT(ParseNumber("0b_._"), Holds<std::string>());
  EXPECT(ParseNumber("0b."), Holds<std::string>());
  // TODO overflow and underflow
}

TEST(Base8Real) {
  EXPECT(ParseNumber("0o0.0"), Holds(0.0));
  EXPECT(ParseNumber("0o.0"), Holds(0.0));
  EXPECT(ParseNumber("0o1."), Holds(1.0));
  EXPECT(ParseNumber("0o1.0"), Holds(1.0));
  EXPECT(ParseNumber("0o.1"), Holds(0.125));
  EXPECT(ParseNumber("0o.001"), Holds(0.001953125));
  EXPECT(ParseNumber("0o.04"), Holds(0.0625));
  EXPECT(ParseNumber("0o_1__1_.2_"), Holds(9.25));
  EXPECT(ParseNumber("0o_._"), Holds<std::string>());
  EXPECT(ParseNumber("0o."), Holds<std::string>());
  // TODO overflow and underflow
}

TEST(Base10Real) {
  EXPECT(ParseNumber("0.0"), Holds(0.0));
  EXPECT(ParseNumber(".0"), Holds(0.0));
  EXPECT(ParseNumber("1."), Holds(1.0));
  EXPECT(ParseNumber("1.0"), Holds(1.0));
  EXPECT(ParseNumber(".1"), Holds(0.1));
  EXPECT(ParseNumber(".001"), Holds(0.001));
  EXPECT(ParseNumber(".04"), Holds(0.04));
  EXPECT(ParseNumber("_1__1_.2_"), Holds(11.2));

  EXPECT(ParseNumber("0d0.0"), Holds(0.0));
  EXPECT(ParseNumber("0d.0"), Holds(0.0));
  EXPECT(ParseNumber("0d1."), Holds(1.0));
  EXPECT(ParseNumber("0d1.0"), Holds(1.0));
  EXPECT(ParseNumber("0d.1"), Holds(0.1));
  EXPECT(ParseNumber("0d.001"), Holds(0.001));
  EXPECT(ParseNumber("0d.04"), Holds(0.04));
  EXPECT(ParseNumber("0d_1__1_.2_"), Holds(11.2));
  EXPECT(ParseNumber("0d_._"), Holds<std::string>());
  EXPECT(ParseNumber("0d."), Holds<std::string>());
  // TODO overflow and underflow
}

TEST(Base16Real) {
  EXPECT(ParseNumber("0x0.0"), Holds(0.0));
  EXPECT(ParseNumber("0x.0"), Holds(0.0));
  EXPECT(ParseNumber("0x1."), Holds(1.0));
  EXPECT(ParseNumber("0x1.0"), Holds(1.0));
  EXPECT(ParseNumber("0x.1"), Holds(0.0625));
  EXPECT(ParseNumber("0x.04"), Holds(0.015625));
  EXPECT(ParseNumber("0x_1__1_.2_"), Holds(17.125));
  EXPECT(ParseNumber("0x_._"), Holds<std::string>());
  EXPECT(ParseNumber("0x."), Holds<std::string>());

  // TODO overflow and underflow
}
