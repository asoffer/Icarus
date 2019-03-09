#include "core/fn_params.h"

#include "test/test.h"

namespace core {
namespace {
// This structure gives us a way to model ambiguity more general than just
// testing for equality.
constexpr bool Ambiguity(int lhs, int rhs) { return (lhs & rhs) != 0; }

TEST(Creation) {
  FnParams<double> params;
  CHECK(params.size() == 0u);
  params.append("pi", 3.14);
  params.append("", 1234);
  CHECK(params.size() == 2u);
  CHECK(params.at(0) == FnParams<double>::Param("pi", 3.14));
  CHECK(params.at(1) == FnParams<double>::Param("", 1234));
}

TEST(Index) {
  using Param = FnParams<double>::Param;
  FnParams<double> params{Param{"pi", 3.14}, Param{"e", 2.718}, Param{"", 1234},
                          Param{"", 5678}, Param{"phi", 1.618}};

  CHECK(params.size() == 5u);
  CHECK(params.lookup_.at("pi") == 0u);
  CHECK(params.lookup_.at("e") == 1u);
  CHECK(params.lookup_.at("phi") == 4u);
  CHECK(params.lookup_.size() == 3u);
}

TEST(Transform) {
  using Param = FnParams<int>::Param;
  FnParams<int> int_params{Param{"a", 1, MUST_NOT_NAME}, Param{"b", 2}, Param{"", 3}};
  FnParams<double> double_params =
      int_params.Transform([](int n) { return n * 0.5; });

  CHECK(double_params.size() == 3u);
  CHECK(double_params.at(0) == FnParams<double>::Param("a", 0.5, MUST_NOT_NAME));
  CHECK(double_params.at(1) == FnParams<double>::Param("b", 1));
  CHECK(double_params.at(2) == FnParams<double>::Param("", 1.5));

  CHECK(double_params.lookup_.size() == 2u);
  CHECK(double_params.lookup_.at("a") == 0u);
  CHECK(double_params.lookup_.at("b") == 1u);
}

TEST(AmbiguouslyCallable) {
  using Param = FnParams<int>::Param;

  {  // Both empty
    FnParams<int> p1, p2;
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity) == true);
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity) == true);
  }

  {  // One empty
    FnParams<int> p1{Param{"a", 1}};
    FnParams<int> p2;
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity) == false);
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity) == false);
  }

  {  // One empty but has default
    FnParams<int> p1{Param{"a", 1, HAS_DEFAULT}};
    FnParams<int> p2;
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity) == true);
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity) == true);
  }

  {  // Same type, different names
    FnParams<int> p1{Param{"a1", 1}};
    FnParams<int> p2{Param{"a2", 1}};
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity) == true);
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity) == true);
  }

  {  // Same type, same name
    FnParams<int> p{Param{"a1", 1}};
    CHECK(AmbiguouslyCallable(p, p, Ambiguity) == true);
  }

  {  // Same name different types
    FnParams<int> p1{Param{"a", 1}};
    FnParams<int> p2{Param{"a", 2}};
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity) == false);
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity) == false);
  }

  {  // Unambiguous because a parameter would have to be named.
    FnParams<int> p1{Param{"a", 1}, Param{"b", 2, HAS_DEFAULT}};
    FnParams<int> p2{Param{"b", 2}};
    CHECK(AmbiguouslyCallable(p1, p2, Ambiguity) == false);
    CHECK(AmbiguouslyCallable(p2, p1, Ambiguity) == false);
  }
}

}  // namespace
}  // namespace core
