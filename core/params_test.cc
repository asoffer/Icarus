#include "core/params.h"

#include "gtest/gtest.h"

namespace core {
namespace {
constexpr bool Ambiguity(int lhs, int rhs) { return (lhs & rhs) != 0; }

TEST(Params, Creation) {
  Params<double> params;
  EXPECT_EQ(params.size(), 0u);
  params.append("pi", 3.14, MUST_NOT_NAME);
  params.append("", 1234);
  EXPECT_EQ(params.size(), 2u);
  EXPECT_EQ(params.at(0), Param<double>("pi", 3.14, MUST_NOT_NAME));
  EXPECT_EQ(params.at(1), Param<double>("", 1234));
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
  EXPECT_EQ(double_params.at(0), Param<double>("a", 0.5, MUST_NOT_NAME));
  EXPECT_EQ(double_params.at(1), Param<double>("b", 1));
  EXPECT_EQ(double_params.at(2), Param<double>("", 1.5));

  EXPECT_EQ(*double_params.at_or_null("a"), 0u);
  EXPECT_EQ(*double_params.at_or_null("b"), 1u);
}

TEST(Params, AmbiguouslyCallable) {
  {  // Both empty
    Params<int> p1, p2;
    EXPECT_TRUE(AmbiguouslyCallable(p1, p2, Ambiguity));
    EXPECT_TRUE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  {  // One empty
    Params<int> p1{Param<int>{"a", 1}};
    Params<int> p2;
    EXPECT_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
    EXPECT_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  {  // One empty but has default
    Params<int> p1{Param<int>{"a", 1, HAS_DEFAULT}};
    Params<int> p2;
    EXPECT_TRUE(AmbiguouslyCallable(p1, p2, Ambiguity));
    EXPECT_TRUE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  {  // Same type, different names
    Params<int> p1{Param<int>{"a1", 1}};
    Params<int> p2{Param<int>{"a2", 1}};
    EXPECT_TRUE(AmbiguouslyCallable(p1, p2, Ambiguity));
    EXPECT_TRUE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  {  // Same type, same name
    Params<int> p{Param<int>{"a1", 1}};
    EXPECT_TRUE(AmbiguouslyCallable(p, p, Ambiguity));
  }

  {  // Same name different types
    Params<int> p1{Param<int>{"a", 1}};
    Params<int> p2{Param<int>{"a", 2}};
    EXPECT_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
    EXPECT_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  {  // Unambiguous because a parameter would have to be named.
    Params<int> p1{Param<int>{"a", 1}, Param<int>{"b", 2, HAS_DEFAULT}};
    Params<int> p2{Param<int>{"b", 2}};
    EXPECT_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
    EXPECT_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  {  // Both defaultable of different types
    Params<int> p1{Param<int>{"a", 1, HAS_DEFAULT}};
    Params<int> p2{Param<int>{"b", 2, HAS_DEFAULT}};
    EXPECT_TRUE(AmbiguouslyCallable(p1, p2, Ambiguity));
    EXPECT_TRUE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  // TODO Many many more tests covering MUST_NOT_NAME
  {  // Anonymous
    Params<int> p1{Param<int>{"", 1, MUST_NOT_NAME}};
    Params<int> p2{Param<int>{"", 2, MUST_NOT_NAME}};
    EXPECT_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
  }
}

TEST(Params, Set) {
  Params<int> p(2);

  EXPECT_EQ(p.at_or_null("n"), nullptr);

  p.set(1, Param<int>("n", 3));
  EXPECT_TRUE(p.at_or_null("n") != nullptr);
}

}  // namespace
}  // namespace core
