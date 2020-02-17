#include "core/params_ref.h"
#include "core/fn_args.h"
#include "core/params.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

using ::testing::ElementsAre;

TEST(ParamsRef, Iteration) {
  Params<int> params{Param<int>{"a", 1}, Param<int>{"b", 2}, Param<int>{"", 3},
                     Param<int>{"", 4}, Param<int>{"e", 5}};
  ParamsRef<int> ref(params);

  EXPECT_THAT(ref, ElementsAre(Param<int>{"a", 1}, Param<int>{"b", 2},
                               Param<int>{"", 3}, Param<int>{"", 4},
                               Param<int>{"e", 5}));
}

TEST(ParamsRef, RemovePrefix) {
  Params<int> params{Param<int>{"a", 1}, Param<int>{"b", 2}, Param<int>{"", 3},
                     Param<int>{"", 4}, Param<int>{"e", 5}};
  ParamsRef<int> ref(params);
  ref.remove_prefix(0);

  EXPECT_THAT(ref, ElementsAre(Param<int>{"a", 1}, Param<int>{"b", 2},
                               Param<int>{"", 3}, Param<int>{"", 4},
                               Param<int>{"e", 5}));

  ref.remove_prefix(1);

  EXPECT_THAT(ref, ElementsAre(Param<int>{"b", 2}, Param<int>{"", 3},
                               Param<int>{"", 4}, Param<int>{"e", 5}));
}

TEST(IsCallable, EmptyParams) {
  auto convertible = [](int from, int to) { return from == to; };

  Params<int> p;
  ParamsRef<int> p_ref = p;
  EXPECT_TRUE(IsCallable(p_ref, FnArgs<int>{}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{3}, {}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{}, {{"a", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{3}, {{"a", 4}}}, convertible));
}

TEST(IsCallable, OneParamWithoutDefault) {
  auto convertible = [](int from, int to) { return from == to; };
  Params<int> p{Param<int>{"a", 4}};
  ParamsRef<int> p_ref = p;
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{3}, {}}, convertible));
  EXPECT_TRUE(IsCallable(p_ref, FnArgs<int>{{4}, {}}, convertible));
  EXPECT_TRUE(IsCallable(p_ref, FnArgs<int>{{}, {{"a", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{}, {{"b", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{4}, {{"a", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{4}, {{"a", 5}}}, convertible));
}

TEST(IsCallable, OneParamWithDefault) {
  auto convertible = [](int from, int to) { return from == to; };
  Params<int> p{Param<int>{"a", 4, HAS_DEFAULT}};
  ParamsRef<int> p_ref = p;
  EXPECT_TRUE(IsCallable(p_ref, FnArgs<int>{}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{3}, {}}, convertible));
  EXPECT_TRUE(IsCallable(p_ref, FnArgs<int>{{4}, {}}, convertible));
  EXPECT_TRUE(IsCallable(p_ref, FnArgs<int>{{}, {{"a", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{}, {{"b", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{4}, {{"a", 4}}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{4}, {{"a", 5}}}, convertible));
}

TEST(IsCallable, MultipleParamsWithNonTrailingDefault) {
  auto convertible = [](int from, int to) { return from == to; };

  Params<int> p{Param<int>{"a", 4, HAS_DEFAULT}, Param<int>{"b", 7}};
  ParamsRef<int> p_ref = p;
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{3}, {}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{4}, {}}, convertible));
  EXPECT_TRUE(IsCallable(p_ref, FnArgs<int>{{4, 7}, {}}, convertible));
  EXPECT_FALSE(IsCallable(p_ref, FnArgs<int>{{}, {{"a", 4}}}, convertible));
  EXPECT_TRUE(IsCallable(p_ref, FnArgs<int>{{4}, {{"b", 7}}}, convertible));
  EXPECT_TRUE(
      IsCallable(p_ref, FnArgs<int>{{}, {{"a", 4}, {"b", 7}}}, convertible));
}

// TODO tests for transform
// TODO tests for FillMissingArgs

}  // namespace
}  // namespace core
