#include "type/variant.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "type/primitive.h"
#include "type/tuple.h"

namespace {
using ::testing::ElementsAre;
using ::testing::IsEmpty;

TEST(Variant, Empty) {
  EXPECT_EQ(type::Var({}), type::Void());
  // Second time because of caching.
  EXPECT_EQ(type::Var({}), type::Void());
  type::Type const* v = type::Var({});
  EXPECT_EQ(type::Var({}), v);
}

TEST(Variant, One) {
  EXPECT_NE(type::Var({type::Int64}), type::Void());
  EXPECT_EQ(type::Var({type::Int64}), type::Var({type::Int64}));
  EXPECT_EQ(type::Var({type::Int64}), type::Int64);
  // Second time because of caching.
  EXPECT_EQ(type::Var({type::Int64}), type::Int64);
  EXPECT_EQ(type::Var({type::Int64, type::Int64}), type::Int64);
}

TEST(Variant, Many) {
  EXPECT_NE(type::Var({type::Int64, type::Bool}), type::Void());
  EXPECT_EQ(type::Var({type::Int64, type::Bool}),
            type::Var({type::Bool, type::Int64}));
  // Second time because of caching.
  EXPECT_EQ(type::Var({type::Int64, type::Bool}),
            type::Var({type::Bool, type::Int64}));
  EXPECT_EQ(type::Var({type::Bool, type::Int64, type::Bool}),
            type::Var({type::Bool, type::Int64}));
}

TEST(Variant, Flattening) {
  EXPECT_EQ(type::Var({type::Var({type::Int64, type::Bool}), type::Bool}),
            type::Var({type::Bool, type::Int64}));
  EXPECT_EQ(type::Var({type::Var({type::Int64, type::Bool}),
                       type::Var({type::Float32, type::Bool})}),
            type::Var({type::Bool, type::Float32, type::Int64}));
}

TEST(Variant, MultiVar) {
  //   EXPECT_DEATH({
  //       type::MultiVar(
  //           {std::vector{type::Int64}, std::vector{type::Int64,
  //           type::Bool}}),
  //   });

  EXPECT_THAT(type::MultiVar({}), IsEmpty());

  auto v1 = std::vector{type::Int64, type::Bool};
  auto v2 = std::vector{type::Int64, type::Bool};
  EXPECT_THAT(
      type::MultiVar({absl::MakeConstSpan(v1), absl::MakeConstSpan(v2)}),
      ElementsAre(type::Int64, type::Bool));

  auto v3 = std::vector{type::Int64, type::Bool};
  auto v4 = std::vector{type::Int64, type::Int64};
  EXPECT_THAT(
      type::MultiVar({absl::MakeConstSpan(v3), absl::MakeConstSpan(v4)}),
      ElementsAre(type::Int64, type::Var({type::Bool, type::Int64})));
}

TEST(Variant, Contains) {
  type::Variant const& var =
      type::Var({type::Int32, type::Int64})->as<type::Variant>();
  EXPECT_FALSE(var.contains(type::Bool));
  EXPECT_TRUE(var.contains(type::Int32));
  EXPECT_FALSE(var.contains(&var));
}

}  // namespace
