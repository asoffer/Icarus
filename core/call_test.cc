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

TEST(Callability, EmptyParams) {
  auto convertible = [](int from, int to) { return from == to; };

  Params<int> p;
  EXPECT_EQ(Callability(p, Arguments<int>{}, convertible),
            CallabilityResult::Valid{});
  EXPECT_EQ(Callability(p, Arguments<int>{{3}, {}}, convertible),
            (CallabilityResult::TooManyArguments{.num_provided     = 1,
                                                .max_num_accepted = 0}));
  EXPECT_EQ(Callability(p, Arguments<int>{{}, {{"a", 4}}}, convertible),
            (CallabilityResult::TooManyArguments{.num_provided     = 1,
                                                .max_num_accepted = 0}));
  EXPECT_EQ(Callability(p, Arguments<int>{{3}, {{"a", 4}}}, convertible),
            (CallabilityResult::TooManyArguments{.num_provided     = 2,
                                                .max_num_accepted = 0}));
}

TEST(Callability, OneParamWithoutDefault) {
  auto convertible = [](int from, int to) { return from == to; };
  Params<int> p{Param<int>{"a", 4}};
  EXPECT_EQ(Callability(p, Arguments<int>{}, convertible),
            CallabilityResult::MissingNonDefaultableArguments{.names = {"a"}});
  EXPECT_EQ(Callability(p, Arguments<int>{{3}, {}}, convertible),
            (CallabilityResult::TypeMismatch{.parameter = 0u, .argument = 0u}));
  EXPECT_EQ(Callability(p, Arguments<int>{{4}, {}}, convertible),
            CallabilityResult::Valid{});
  EXPECT_EQ(Callability(p, Arguments<int>{{}, {{"a", 4}}}, convertible),
            CallabilityResult::Valid{});
  EXPECT_EQ(Callability(p, Arguments<int>{{}, {{"b", 4}}}, convertible),
            CallabilityResult::NoParameterNamed{.name = "b"});
  EXPECT_EQ(Callability(p, Arguments<int>{{4}, {{"a", 4}}}, convertible),
            (CallabilityResult::TooManyArguments{.num_provided     = 2,
                                                .max_num_accepted = 1}));
  EXPECT_EQ(Callability(p, Arguments<int>{{4}, {{"a", 5}}}, convertible),
            (CallabilityResult::TooManyArguments{.num_provided     = 2,
                                                .max_num_accepted = 1}));
}

TEST(Callability, OneParamWithDefault) {
  auto convertible = [](int from, int to) { return from == to; };
  Params<int> p{Param<int>{"a", 4, HAS_DEFAULT}};
  EXPECT_EQ(Callability(p, Arguments<int>{}, convertible),
            CallabilityResult::Valid{});
  EXPECT_EQ(Callability(p, Arguments<int>{{3}, {}}, convertible),
            (CallabilityResult::TypeMismatch{.parameter = 0u, .argument = 0u}));
  EXPECT_EQ(Callability(p, Arguments<int>{{4}, {}}, convertible),
            CallabilityResult::Valid{});
  EXPECT_EQ(Callability(p, Arguments<int>{{}, {{"a", 4}}}, convertible),
            CallabilityResult::Valid{});
  EXPECT_EQ(Callability(p, Arguments<int>{{}, {{"b", 4}}}, convertible),
               CallabilityResult::NoParameterNamed{.name = "b"});
  EXPECT_EQ(Callability(p, Arguments<int>{{4}, {{"a", 4}}}, convertible),
            (CallabilityResult::TooManyArguments{.num_provided     = 2,
                                                .max_num_accepted = 1}));
  EXPECT_EQ(Callability(p, Arguments<int>{{4}, {{"a", 5}}}, convertible),
            (CallabilityResult::TooManyArguments{.num_provided     = 2,
                                                .max_num_accepted = 1}));
}

TEST(Callability, MultipleParamsWithNonTrailingDefault) {
  auto convertible = [](int from, int to) { return from == to; };

  Params<int> p{Param<int>{"a", 4, HAS_DEFAULT}, Param<int>{"b", 7}};
  EXPECT_EQ(Callability(p, Arguments<int>{}, convertible),
            CallabilityResult::MissingNonDefaultableArguments{.names = {"b"}});
  EXPECT_EQ(Callability(p, Arguments<int>{{3}, {}}, convertible),
            (CallabilityResult::TypeMismatch{.parameter = 0u, .argument = 0u}));
  EXPECT_EQ(Callability(p, Arguments<int>{{4}, {}}, convertible),
            CallabilityResult::MissingNonDefaultableArguments{.names = {"b"}});
  EXPECT_EQ(Callability(p, Arguments<int>{{4, 7}, {}}, convertible),
            CallabilityResult::Valid{});
  EXPECT_EQ(Callability(p, Arguments<int>{{}, {{"a", 4}}}, convertible),
            CallabilityResult::MissingNonDefaultableArguments{.names = {"b"}});
  EXPECT_EQ(Callability(p, Arguments<int>{{4}, {{"b", 7}}}, convertible),
            CallabilityResult::Valid{});
  EXPECT_EQ(
      Callability(p, Arguments<int>{{}, {{"a", 4}, {"b", 7}}}, convertible),
      CallabilityResult::Valid{});
}

TEST(Callability, MultipleParametersWithDefaults) {
  auto convertible = [](int from, int to) { return from == to; };

  Params<int> p{Param<int>{"a", 1, HAS_DEFAULT},
                Param<int>{"b", 2, HAS_DEFAULT}};

  EXPECT_EQ(Callability(p, Arguments<int>{}, convertible),
            CallabilityResult::Valid{});
  EXPECT_EQ(
      Callability(p, Arguments<int>{{1}, {{"a", 1}}}, convertible),
      (CallabilityResult::PositionalArgumentNamed{.index = 0, .name = "a"}));
}

}  // namespace
}  // namespace core
