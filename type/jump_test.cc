#include "type/jump.h"

#include "core/params.h"
#include "gtest/gtest.h"
#include "type/fake.h"

namespace {

TEST(Jump, Stateless) {
  auto const *j1 = type::Jmp(
      nullptr,
      core::Params<type::Type const *>{core::Param("x", type::Fake<1, 2>()),
                                       core::Param("y", type::Fake<3, 4>())});
  auto const *j2 = type::Jmp(
      nullptr,
      core::Params<type::Type const *>{core::Param("x", type::Fake<1, 2>()),
                                       core::Param("y", type::Fake<3, 4>())});
  auto const *j3 = type::Jmp(
      nullptr,
      core::Params<type::Type const *>{core::Param("y", type::Fake<1, 2>()),
                                       core::Param("x", type::Fake<3, 4>())});
  EXPECT_EQ(j1, j2);
  EXPECT_NE(j1, j3);
  EXPECT_EQ(j1->state(), nullptr);
  EXPECT_EQ(j3->state(), nullptr);
}

TEST(Jump, Statefull) {
  auto const *j1 = type::Jmp(
      type::Fake<1, 1>(),
      core::Params<type::Type const *>{core::Param("x", type::Fake<1, 2>()),
                                       core::Param("y", type::Fake<3, 4>())});
  auto const *j2 = type::Jmp(
      type::Fake<1, 1>(),
      core::Params<type::Type const *>{core::Param("x", type::Fake<1, 2>()),
                                       core::Param("y", type::Fake<3, 4>())});
  auto const *j3 = type::Jmp(
      type::Fake<1, 1>(),
      core::Params<type::Type const *>{core::Param("y", type::Fake<1, 2>()),
                                       core::Param("x", type::Fake<3, 4>())});
  EXPECT_EQ(j1, j2);
  EXPECT_NE(j1, j3);
  auto const *t = type::Fake<1, 1>();
  EXPECT_EQ(j1->state(), t);
  EXPECT_EQ(j3->state(), t);
}

TEST(Jump, ToString) {
  auto const *j1 = type::Jmp(
      nullptr,
      core::Params<type::Type const *>{core::Param("x", type::Fake<1, 2>()),
                                       core::Param("y", type::Fake<3, 4>())});
  EXPECT_EQ(j1->to_string(), "jump (x: Fake(1, 2), y: Fake(3, 4))");

  auto const *j2 = type::Jmp(
      type::Fake<1, 1>(),
      core::Params<type::Type const *>{core::Param("x", type::Fake<1, 2>()),
                                       core::Param("y", type::Fake<3, 4>())});
  EXPECT_EQ(j2->to_string(),
            "jump [Fake(1, 1)] (x: Fake(1, 2), y: Fake(3, 4))");
}

}  // namespace
