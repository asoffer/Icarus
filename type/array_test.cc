#include "type/array.h"

#include "gtest/gtest.h"
#include "type/fake.h"

namespace {

TEST(Array, Construction) {
  EXPECT_EQ(type::Arr(3, type::Fake<1, 2>()), type::Arr(3, type::Fake<1, 2>()));
  EXPECT_NE(type::Arr(3, type::Fake<1, 2>()), type::Arr(4, type::Fake<1, 2>()));
  EXPECT_NE(type::Arr(3, type::Fake<1, 2>()), type::Arr(3, type::Fake<2, 1>()));

  EXPECT_EQ(type::Arr(3, type::Fake<1, 2>())->bytes(core::Host),
            core::Bytes{6});
  EXPECT_EQ(type::Arr(3, type::Fake<3, 2>())->bytes(core::Host),
            core::Bytes{12});
  EXPECT_EQ(type::Arr(3, type::Fake<3, 4>())->bytes(core::Host),
            core::Bytes{12});
  EXPECT_EQ(type::Arr(3, type::Fake<1, 2>())->alignment(core::Host),
            core::Alignment{2});
}

TEST(Array, ToString) {
  EXPECT_EQ(type::Arr(3, type::Fake<1, 2>())->to_string(), "[3; Fake(1, 2)]");
  EXPECT_EQ(type::Arr(4, type::Arr(3, type::Fake<1, 2>()))->to_string(),
            "[4, 3; Fake(1, 2)]");
}

}  // namespace
