#include "type/slice.h"

#include "gtest/gtest.h"
#include "type/fake.h"

namespace {

TEST(Array, Construction) {
  EXPECT_EQ(type::Slc(type::Fake<1, 2>()), type::Slc(type::Fake<1, 2>()));
  EXPECT_NE(type::Slc(type::Fake<1, 2>()), type::Slc(type::Fake<2, 1>()));

  EXPECT_EQ(type::Slc(type::Fake<1, 2>())->bytes(core::Host), core::Bytes{16});
  EXPECT_EQ(type::Slc(type::Fake<3, 2>())->bytes(core::Host), core::Bytes{16});
  EXPECT_EQ(type::Slc(type::Fake<3, 4>())->bytes(core::Host), core::Bytes{16});
  EXPECT_EQ(type::Slc(type::Fake<1, 2>())->alignment(core::Host),
            core::Alignment{8});
}

TEST(Slcay, ToString) {
  EXPECT_EQ(type::Slc(type::Fake<1, 2>())->to_string(), "[]Fake(1, 2)");
  EXPECT_EQ(type::Slc(type::Slc(type::Fake<1, 2>()))->to_string(),
            "[][]Fake(1, 2)");
}

}  // namespace
