#include "core/fn_params.h"

#include "test/catch.h"

namespace core {
namespace {
constexpr bool Ambiguity(int lhs, int rhs) { return (lhs & rhs) != 0; }

TEST_CASE("creation") {
  FnParams<double> params;
  CHECK(params.size() == 0u);
  params.append("pi", 3.14, MUST_NOT_NAME);
  params.append("", 1234);
  CHECK(params.size() == 2u);
  CHECK(params.at(0) == Param<double>("pi", 3.14, MUST_NOT_NAME));
  CHECK(params.at(1) == Param<double>("", 1234));
}

TEST_CASE("append") {
  FnParams<double> params;
  CHECK(params.size() == 0u);
  params.append("pi", 3.14, MUST_NOT_NAME);
  params.append("", 1234);
}

TEST_CASE("index") {
  FnParams<double> params{Param<double>{"pi", 3.14}, Param<double>{"e", 2.718},
                          Param<double>{"", 1234}, Param<double>{"", 5678},
                          Param<double>{"phi", 1.618}};

  CHECK(params.size() == 5u);
  CHECK(*params.at_or_null("pi") == 0u);
  CHECK(*params.at_or_null("e") == 1u);
  CHECK(*params.at_or_null("phi") == 4u);
}

TEST_CASE("transform") {
  FnParams<int> int_params{Param<int>{"a", 1, MUST_NOT_NAME},
                           Param<int>{"b", 2}, Param<int>{"", 3}};
  FnParams<double> double_params =
      int_params.Transform([](int n) { return n * 0.5; });

  CHECK(double_params.size() == 3u);
  CHECK(double_params.at(0) == Param<double>("a", 0.5, MUST_NOT_NAME));
  CHECK(double_params.at(1) == Param<double>("b", 1));
  CHECK(double_params.at(2) == Param<double>("", 1.5));

  CHECK(*double_params.at_or_null("a") == 0u);
  CHECK(*double_params.at_or_null("b") == 1u);
}

TEST_CASE("ambiguously callable") {
  SECTION("Both empty"){
    FnParams<int> p1, p2;
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  SECTION("One empty"){
    FnParams<int> p1{Param<int>{"a", 1}};
    FnParams<int> p2;
    CHECK_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  SECTION("One empty but has default") {
    FnParams<int> p1{Param<int>{"a", 1, HAS_DEFAULT}};
    FnParams<int> p2;
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  SECTION("Same type, different names") {
    FnParams<int> p1{Param<int>{"a1", 1}};
    FnParams<int> p2{Param<int>{"a2", 1}};
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  SECTION("Same type, same name") {
    FnParams<int> p{Param<int>{"a1", 1}};
    CHECK(AmbiguouslyCallable(p, p, Ambiguity));
  }

  SECTION("Same name different types") {
    FnParams<int> p1{Param<int>{"a", 1}};
    FnParams<int> p2{Param<int>{"a", 2}};
    CHECK_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  SECTION("Unambiguous because a parameter would have to be named.") {
    FnParams<int> p1{Param<int>{"a", 1}, Param<int>{"b", 2, HAS_DEFAULT}};
    FnParams<int> p2{Param<int>{"b", 2}};
    CHECK_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  SECTION("Both defaultable of different types") {
    FnParams<int> p1{Param<int>{"a", 1, HAS_DEFAULT}};
    FnParams<int> p2{Param<int>{"b", 2, HAS_DEFAULT}};
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity));
  }
}

TEST_CASE("set") {
  FnParams<int> p(2);

  CHECK(p.at_or_null("n") == nullptr);

  p.set(1, Param<int>("n", 3));
  CHECK(p.at_or_null("n") != nullptr);
}

TEST_CASE("IsCallable") {
  auto convertible = [](int from, int to) { return from == to; };

  SECTION("Empty params") {
    FnParams<int> p;
    CHECK(IsCallable(p, FnArgs<int>{}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{3}, {}}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{}, {{"a", 4}}}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{3}, {{"a", 4}}}, convertible));
  }

  SECTION("One param without default") {
    FnParams<int> p{Param<int>{"a", 4}};
    CHECK_FALSE(IsCallable(p, FnArgs<int>{}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{3}, {}}, convertible));
    CHECK(IsCallable(p, FnArgs<int>{{4}, {}}, convertible));
    CHECK(IsCallable(p, FnArgs<int>{{}, {{"a", 4}}}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{}, {{"b", 4}}}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{4}, {{"a", 4}}}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{4}, {{"a", 5}}}, convertible));
  }

  SECTION("One param with default") {
    FnParams<int> p{Param<int>{"a", 4, HAS_DEFAULT}};
    CHECK(IsCallable(p, FnArgs<int>{}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{3}, {}}, convertible));
    CHECK(IsCallable(p, FnArgs<int>{{4}, {}}, convertible));
    CHECK(IsCallable(p, FnArgs<int>{{}, {{"a", 4}}}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{}, {{"b", 4}}}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{4}, {{"a", 4}}}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{4}, {{"a", 5}}}, convertible));
  }

  SECTION("Multiple parameters with non-trailing default") {
    FnParams<int> p{Param<int>{"a", 4, HAS_DEFAULT}, Param<int>{"b", 7}};
    CHECK_FALSE(IsCallable(p, FnArgs<int>{}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{3}, {}}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{4}, {}}, convertible));
    CHECK(IsCallable(p, FnArgs<int>{{4, 7}, {}}, convertible));
    CHECK_FALSE(IsCallable(p, FnArgs<int>{{}, {{"a", 4}}}, convertible));
    CHECK(IsCallable(p, FnArgs<int>{{4}, {{"b", 7}}}, convertible));
    CHECK(IsCallable(p, FnArgs<int>{{}, {{"a", 4}, {"b", 7}}}, convertible));
  }
}

}  // namespace
}  // namespace core
