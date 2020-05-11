#include "ir/value/value.h"

#include <sstream>
#include <cstdint>

#include "gtest/gtest.h"
#include "gmock/gmock.h"
#include "ir/value/reg_or.h"

namespace {
using ::testing::ElementsAre;

TEST(Value, BasicConstruction) {
  EXPECT_TRUE(ir::Value(true).get<bool>());
  EXPECT_FALSE(ir::Value(false).get<bool>());
  EXPECT_EQ(ir::Value(3).get<int32_t>(), 3);
  EXPECT_EQ(ir::Value(3.0).get<double>(), 3.0);
}

TEST(Value, RegOrConstruction) {
  EXPECT_EQ(ir::Value(ir::RegOr<int32_t>(3)).get<int32_t>(), 3);
  EXPECT_EQ(ir::Value(ir::RegOr<int32_t>(ir::Reg(3))).get<ir::Reg>(),
            ir::Reg(3));
}

TEST(Value, GetIf) {
  {
    ir::Value v(int32_t{3});
    ASSERT_NE(v.get_if<int32_t>(), nullptr);
    EXPECT_EQ(*v.get_if<int32_t>(), 3);
    EXPECT_EQ(v.get_if<bool>(), nullptr);
  }

  {
    ir::Value const v(int32_t{3});
    ASSERT_NE(v.get_if<int32_t>(), nullptr);
    EXPECT_EQ(*v.get_if<int32_t>(), 3);
    EXPECT_EQ(v.get_if<bool>(), nullptr);
  }
}

TEST(Value, Get) {
  {
    ir::Value v(int32_t{3});
    EXPECT_EQ(v.get<int32_t>(), 3);
    EXPECT_EQ(v.get<ir::RegOr<int32_t>>(), ir::RegOr<int32_t>(3));
  }

  {
    ir::Value const v(int32_t{3});
    EXPECT_EQ(v.get<int32_t>(), 3);
    EXPECT_EQ(v.get<ir::RegOr<int32_t>>(), ir::RegOr<int32_t>(3));
  }

  {
    ir::Value v(ir::Reg(3));
    EXPECT_EQ(v.get<ir::RegOr<int32_t>>(), ir::RegOr<int32_t>(ir::Reg(3)));
    EXPECT_EQ(v.get<ir::RegOr<bool>>(), ir::RegOr<bool>(ir::Reg(3)));
  }
}

TEST(MultiValue, Basic) {
  ir::MultiValue m(
      std::vector{ir::Value(3), ir::Value(true), ir::Value(ir::Reg(3))});
  ASSERT_EQ(m.size(), 3);
  EXPECT_EQ(m[0], ir::Value(3));
  EXPECT_EQ(m[1], ir::Value(true));
  EXPECT_EQ(m[2], ir::Value(ir::Reg(3)));

  EXPECT_THAT(m.span(),
              ElementsAre(ir::Value(3), ir::Value(true), ir::Value(ir::Reg(3))));
}

TEST(MultiValue, Copy) {
  ir::MultiValue m(
      std::vector{ir::Value(3), ir::Value(true), ir::Value(ir::Reg(3))});
  auto copy = m;
  ASSERT_EQ(copy.size(), 3);
  EXPECT_EQ(copy[0], ir::Value(3));
  EXPECT_EQ(copy[1], ir::Value(true));
  EXPECT_EQ(copy[2], ir::Value(ir::Reg(3)));

  EXPECT_THAT(copy.span(), ElementsAre(ir::Value(3), ir::Value(true),
                                       ir::Value(ir::Reg(3))));
  m = copy;

  ASSERT_EQ(m.size(), 3);
  EXPECT_EQ(m[0], ir::Value(3));
  EXPECT_EQ(m[1], ir::Value(true));
  EXPECT_EQ(m[2], ir::Value(ir::Reg(3)));

  EXPECT_THAT(m.span(), ElementsAre(ir::Value(3), ir::Value(true),
                                    ir::Value(ir::Reg(3))));
}

TEST(MultiValue, Move) {
  ir::MultiValue m(
      std::vector{ir::Value(3), ir::Value(true), ir::Value(ir::Reg(3))});
  auto move = std::move(m);
  ASSERT_EQ(move.size(), 3);
  EXPECT_EQ(move[0], ir::Value(3));
  EXPECT_EQ(move[1], ir::Value(true));
  EXPECT_EQ(move[2], ir::Value(ir::Reg(3)));

  EXPECT_THAT(move.span(), ElementsAre(ir::Value(3), ir::Value(true),
                                       ir::Value(ir::Reg(3))));
  m = std::move(move);

  ASSERT_EQ(m.size(), 3);
  EXPECT_EQ(m[0], ir::Value(3));
  EXPECT_EQ(m[1], ir::Value(true));
  EXPECT_EQ(m[2], ir::Value(ir::Reg(3)));

  EXPECT_THAT(m.span(), ElementsAre(ir::Value(3), ir::Value(true),
                                       ir::Value(ir::Reg(3))));
}

TEST(MultiValue, Equality) {
  EXPECT_EQ(ir::MultiValue(std::vector{ir::Value(3), ir::Value(true),
                                       ir::Value(ir::Reg(3))}),
            ir::MultiValue(std::vector{ir::Value(3), ir::Value(true),
                                       ir::Value(ir::Reg(3))}));
  EXPECT_NE(ir::MultiValue(std::vector{ir::Value(3), ir::Value(true),
                                       ir::Value(ir::Reg(4))}),
            ir::MultiValue(std::vector{ir::Value(3), ir::Value(true),
                                       ir::Value(ir::Reg(3))}));
  EXPECT_NE(ir::MultiValue(std::vector{ir::Value(3), ir::Value(true),
                                       ir::Value(ir::Reg(3)), ir::Value(true)}),
            ir::MultiValue(std::vector{ir::Value(3), ir::Value(true),
                                       ir::Value(ir::Reg(3))}));

  EXPECT_NE(
      ir::MultiValue(std::vector{
          ir::Value(3), ir::Value(ir::MultiValue(std::vector{ir::Value(true)})),
          ir::Value(ir::Reg(3))}),
      ir::MultiValue(
          std::vector{ir::Value(3), ir::Value(true), ir::Value(ir::Reg(3))}));
}

TEST(MultiValue, InValue) {
  ir::MultiValue m(
      std::vector{ir::Value(3), ir::Value(true), ir::Value(ir::Reg(3))});
  ir::Value v(m);
  ASSERT_NE(v.get_if<ir::MultiValue>(), nullptr);
  EXPECT_EQ(v.get<ir::MultiValue>(), m);
}

}  // namespace
