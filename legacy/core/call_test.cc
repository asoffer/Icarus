#include "core/call.h"

#include "gtest/gtest.h"

namespace core {
namespace {

constexpr bool Ambiguity(int lhs, int rhs) { return (lhs & rhs) != 0; }

TEST(AmbiguouslyCallable, BothEmpty) {
  Parameters<int> p1, p2;
  EXPECT_TRUE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_TRUE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

TEST(AmbiguouslyCallable, OneEmpty) {
  Parameters<int> p1{Parameter<int>{.name = "a", .value = 1}};
  Parameters<int> p2;
  EXPECT_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

TEST(AmbiguouslyCallable, OneEmptyWithDefault) {
  Parameters<int> p1{Parameter<int>{
      .name = "a", .value = 1, .flags = ParameterFlags::HasDefault()}};
  Parameters<int> p2;
  EXPECT_TRUE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_TRUE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

TEST(AmbiguouslyCallable, SameTypeDifferentNames) {
  Parameters<int> p1{Parameter<int>{.name = "a1", .value = 1}};
  Parameters<int> p2{Parameter<int>{.name = "a2", .value = 1}};
  EXPECT_TRUE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_TRUE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

TEST(AmbiguouslyCallable, SameTypeSameName) {
  Parameters<int> p{Parameter<int>{.name = "a1", .value = 1}};
  EXPECT_TRUE(AmbiguouslyCallable(p, p, Ambiguity));
}

TEST(AmbiguouslyCallable, SameNameDifferentTypes) {
  Parameters<int> p1{Parameter<int>{.name = "a", .value = 1}};
  Parameters<int> p2{Parameter<int>{.name = "a", .value = 2}};
  EXPECT_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

TEST(AmbiguouslyCallable, UnambiguousBecauseParamaterNeedsName) {
  Parameters<int> p1{
      Parameter<int>{.name = "a", .value = 1},
      Parameter<int>{
          .name = "b", .value = 2, .flags = ParameterFlags::HasDefault()}};
  Parameters<int> p2{Parameter<int>{.name = "b", .value = 2}};
  EXPECT_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

TEST(AmbiguouslyCallable, BothDefaultableDifferentTypes) {
  Parameters<int> p1{Parameter<int>{
      .name = "a", .value = 1, .flags = ParameterFlags::HasDefault()}};
  Parameters<int> p2{Parameter<int>{
      .name = "b", .value = 2, .flags = ParameterFlags::HasDefault()}};
  EXPECT_TRUE(AmbiguouslyCallable(p1, p2, Ambiguity));
  EXPECT_TRUE(AmbiguouslyCallable(p2, p1, Ambiguity));
}

// TODO Many many more tests covering ParameterFlags::MustNotName()
TEST(AmbiguouslyCallable, Anonymous) {
  Parameters<int> p1{Parameter<int>{
      .name = "", .value = 1, .flags = ParameterFlags::MustNotName()}};
  Parameters<int> p2{Parameter<int>{
      .name = "", .value = 2, .flags = ParameterFlags::MustNotName()}};
  EXPECT_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
}

TEST(Callability, EmptyParameters) {
  auto convertible = [](int from, int to) { return from == to; };

  Parameters<int> p;
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
  Parameters<int> p{Parameter<int>{.name = "a", .value = 4}};
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
  Parameters<int> p{Parameter<int>{
      .name = "a", .value = 4, .flags = ParameterFlags::HasDefault()}};
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

TEST(Callability, MultipleParametersWithNonTrailingDefault) {
  auto convertible = [](int from, int to) { return from == to; };

  Parameters<int> p{
      Parameter<int>{
          .name = "a", .value = 4, .flags = ParameterFlags::HasDefault()},
      Parameter<int>{.name = "b", .value = 7}};
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

  Parameters<int> p{
      Parameter<int>{
          .name = "a", .value = 1, .flags = ParameterFlags::HasDefault()},
      Parameter<int>{
          .name = "b", .value = 2, .flags = ParameterFlags::HasDefault()}};

  EXPECT_EQ(Callability(p, Arguments<int>{}, convertible),
            CallabilityResult::Valid{});
  EXPECT_EQ(
      Callability(p, Arguments<int>{{1}, {{"a", 1}}}, convertible),
      (CallabilityResult::PositionalArgumentNamed{.index = 0, .name = "a"}));
}

}  // namespace
}  // namespace core
