#include "core/call.h"

#include "gtest/gtest.h"

namespace core {
namespace {

constexpr bool Ambiguity(int lhs, int rhs) { return (lhs & rhs) != 0; }

TEST(AmbiguouslyCallable, BothEmpty) {
  Params<int> p1, p2;
  EXPECT_TRUE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_TRUE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

TEST(AmbiguouslyCallable, OneEmpty) {
  Params<int> p1{Param<int>{"a", 1}};
  Params<int> p2;
  EXPECT_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

TEST(AmbiguouslyCallable, OneEmptyWithDefault) {
  Params<int> p1{Param<int>{"a", 1, HAS_DEFAULT}};
  Params<int> p2;
  EXPECT_TRUE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_TRUE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

TEST(AmbiguouslyCallable, SameTypeDifferentNames) {
  Params<int> p1{Param<int>{"a1", 1}};
  Params<int> p2{Param<int>{"a2", 1}};
  EXPECT_TRUE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_TRUE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

TEST(AmbiguouslyCallable, SameTypeSameName) {
  Params<int> p{Param<int>{"a1", 1}};
  EXPECT_TRUE(AmbiguouslyCallable(p, p, Ambiguity));
}

TEST(AmbiguouslyCallable, SameNameDifferentTypes) {
  Params<int> p1{Param<int>{"a", 1}};
  Params<int> p2{Param<int>{"a", 2}};
  EXPECT_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

TEST(AmbiguouslyCallable, UnambiguousBecauseParamaterNeedsName) {
  Params<int> p1{Param<int>{"a", 1}, Param<int>{"b", 2, HAS_DEFAULT}};
  Params<int> p2{Param<int>{"b", 2}};
  EXPECT_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

TEST(AmbiguouslyCallable, BothDefaultableDifferentTypes) {
  Params<int> p1{Param<int>{"a", 1, HAS_DEFAULT}};
  Params<int> p2{Param<int>{"b", 2, HAS_DEFAULT}};
  EXPECT_TRUE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_TRUE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

// TODO Many many more tests covering MUST_NOT_NAME
TEST(AmbiguouslyCallable, Anonymous) {
  Params<int> p1{Param<int>{"", 1, MUST_NOT_NAME}};
  Params<int> p2{Param<int>{"", 2, MUST_NOT_NAME}};
  EXPECT_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
}

TEST(IsCallable, EmptyParams) {
  auto convertible = [](int from, int to) { return from == to; };

  Params<int> p;
  ParamsRef<int> p_ref = p;
  EXPECT_TRUE(IsCallable(p_ref, Arguments<int>{}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{3}, {}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{}, {{"a", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{3}, {{"a", 4}}}, convertible));
}

TEST(IsCallable, OneParamWithoutDefault) {
  auto convertible = [](int from, int to) { return from == to; };
  Params<int> p{Param<int>{"a", 4}};
  ParamsRef<int> p_ref = p;
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{3}, {}}, convertible));
  EXPECT_TRUE(IsCallable(p_ref, Arguments<int>{{4}, {}}, convertible));
  EXPECT_TRUE(IsCallable(p_ref, Arguments<int>{{}, {{"a", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{}, {{"b", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{4}, {{"a", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{4}, {{"a", 5}}}, convertible));
}

TEST(IsCallable, OneParamWithDefault) {
  auto convertible = [](int from, int to) { return from == to; };
  Params<int> p{Param<int>{"a", 4, HAS_DEFAULT}};
  ParamsRef<int> p_ref = p;
  EXPECT_TRUE(IsCallable(p_ref, Arguments<int>{}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{3}, {}}, convertible));
  EXPECT_TRUE(IsCallable(p_ref, Arguments<int>{{4}, {}}, convertible));
  EXPECT_TRUE(IsCallable(p_ref, Arguments<int>{{}, {{"a", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{}, {{"b", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{4}, {{"a", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{4}, {{"a", 5}}}, convertible));
}

TEST(IsCallable, MultipleParamsWithNonTrailingDefault) {
  auto convertible = [](int from, int to) { return from == to; };

  Params<int> p{Param<int>{"a", 4, HAS_DEFAULT}, Param<int>{"b", 7}};
  ParamsRef<int> p_ref = p;
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{3}, {}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{4}, {}}, convertible));
  EXPECT_TRUE(IsCallable(p_ref, Arguments<int>{{4, 7}, {}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, Arguments<int>{{}, {{"a", 4}}}, convertible));
  EXPECT_TRUE(IsCallable(p_ref, Arguments<int>{{4}, {{"b", 7}}}, convertible));
  EXPECT_TRUE(
      IsCallable(p_ref, Arguments<int>{{}, {{"a", 4}, {"b", 7}}}, convertible));
}

// TODO tests for transform
// TODO tests for FillMissingArgs

}  // namespace
}  // namespace core
