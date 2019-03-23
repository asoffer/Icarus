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
  CHECK(params.lookup_.at("pi") == 0u);
  CHECK(params.lookup_.at("e") == 1u);
  CHECK(params.lookup_.at("phi") == 4u);
  CHECK(params.lookup_.size() == 3u);
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

  CHECK(double_params.lookup_.size() == 2u);
  CHECK(double_params.lookup_.at("a") == 0u);
  CHECK(double_params.lookup_.at("b") == 1u);
}

TEST_CASE("ambiguously callable") {
  {  // Both empty
    FnParams<int> p1, p2;
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  {  // One empty
    FnParams<int> p1{Param<int>{"a", 1}};
    FnParams<int> p2;
    CHECK_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  {  // One empty but has default
    FnParams<int> p1{Param<int>{"a", 1, HAS_DEFAULT}};
    FnParams<int> p2;
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  {  // Same type, different names
    FnParams<int> p1{Param<int>{"a1", 1}};
    FnParams<int> p2{Param<int>{"a2", 1}};
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  {  // Same type, same name
    FnParams<int> p{Param<int>{"a1", 1}};
    CHECK(AmbiguouslyCallable(p, p, Ambiguity));
  }

  {  // Same name different types
    FnParams<int> p1{Param<int>{"a", 1}};
    FnParams<int> p2{Param<int>{"a", 2}};
    CHECK_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  {  // Unambiguous because a parameter would have to be named.
    FnParams<int> p1{Param<int>{"a", 1}, Param<int>{"b", 2, HAS_DEFAULT}};
    FnParams<int> p2{Param<int>{"b", 2}};
    CHECK_FALSE(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK_FALSE(AmbiguouslyCallable(p2, p1, Ambiguity));
  }

  {  // Both defaultable of different types
    FnParams<int> p1{Param<int>{"a", 1, HAS_DEFAULT}};
    FnParams<int> p2{Param<int>{"b", 2, HAS_DEFAULT}};
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity));
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity));
  }
}

}  // namespace
}  // namespace core
