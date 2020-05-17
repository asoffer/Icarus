#include "core/params.h"

#include "gtest/gtest.h"

namespace core {
namespace {

TEST(Params, Creation) {
  Params<double> params;
  EXPECT_EQ(params.size(), 0u);
  params.append("pi", 3.14, MUST_NOT_NAME);
  params.append("", 1234);
  EXPECT_EQ(params.size(), 2u);
  EXPECT_EQ(params[0], Param<double>("pi", 3.14, MUST_NOT_NAME));
  EXPECT_EQ(params[1], Param<double>("", 1234));
}

TEST(Params, Append) {
  Params<double> params;
  EXPECT_EQ(params.size(), 0u);
  params.append("pi", 3.14, MUST_NOT_NAME);
  params.append("", 1234);
}

TEST(Params, Index) {
  Params<double> params{Param<double>{"pi", 3.14}, Param<double>{"e", 2.718},
                        Param<double>{"", 1234}, Param<double>{"", 5678},
                        Param<double>{"phi", 1.618}};

  EXPECT_EQ(params.size(), 5u);
  EXPECT_EQ(*params.at_or_null("pi"), 0u);
  EXPECT_EQ(*params.at_or_null("e"), 1u);
  EXPECT_EQ(*params.at_or_null("phi"), 4u);
}

TEST(Params, Transform) {
  Params<int> int_params{Param<int>{"a", 1, MUST_NOT_NAME}, Param<int>{"b", 2},
                         Param<int>{"", 3}};
  Params<double> double_params =
      int_params.Transform([](int n) { return n * 0.5; });

  EXPECT_EQ(double_params.size(), 3u);
  EXPECT_EQ(double_params[0], Param<double>("a", 0.5, MUST_NOT_NAME));
  EXPECT_EQ(double_params[1], Param<double>("b", 1));
  EXPECT_EQ(double_params[2], Param<double>("", 1.5));

  EXPECT_EQ(*double_params.at_or_null("a"), 0u);
  EXPECT_EQ(*double_params.at_or_null("b"), 1u);
}

TEST(Params, Set) {
  Params<int> p(2);

  EXPECT_EQ(p.at_or_null("n"), nullptr);

  p.set(1, Param<int>("n", 3));
  EXPECT_TRUE(p.at_or_null("n") != nullptr);
}

}  // namespace
}  // namespace core
