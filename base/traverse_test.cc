#include "base/traverse.h"

#include <list>
#include <string>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace base {
namespace {

using ::testing::ElementsAre;

struct ToUpper {
  void operator()(char& c) {
    if ('a' <= c and c <= 'z') { c += 'A' - 'a'; }
  }

  void operator()(std::string& s) {
    for (char& c : s) { (*this)(c); }
    // Append to it so we can tell the difference between this being invoked
    // versus the implicit container traversal being invoked.
    s.push_back('x');
  }
};

template <typename T>
struct Aggregate {
  T n;
  char c;

  bool operator==(Aggregate const&) const = default;
};

template <size_t N, typename T>
auto& get(Aggregate<T>& a) {
  if constexpr (N == 0) {
    return a.n;
  } else {
    return a.c;
  }
}

}  // namespace
}  // namespace base

namespace std {
template <typename T>
struct tuple_size<::base::Aggregate<T>> {
  static constexpr size_t value = 2;
};

template <typename T>
struct tuple_element<0, ::base::Aggregate<T>> {
  using type = T;
};

template <typename T>
struct tuple_element<1, ::base::Aggregate<T>> {
  using type = char;
};

}  // namespace std

namespace base {
namespace {
TEST(Primitives, Traverse) {
  EXPECT_TRUE((TraversableBy<char, ToUpper>));
  EXPECT_FALSE((TraversableBy<int, ToUpper>));

  ToUpper upper;
  char c = 'a';
  base::Traverse(upper, c);
  EXPECT_EQ(c, 'A');
  base::Traverse(upper, c);
  EXPECT_EQ(c, 'A');
}

TEST(Primitives, TupleLike) {
  EXPECT_TRUE((TraversableBy<std::tuple<char, char>, ToUpper>));
  EXPECT_FALSE((TraversableBy<Aggregate<int>, ToUpper>));
  EXPECT_TRUE((TraversableBy<Aggregate<char>, ToUpper>));

  ToUpper upper;
  std::tuple t('a', 'b');
  base::Traverse(upper, t);
  EXPECT_EQ(t, std::tuple('A', 'B'));
  Aggregate<char> a{.n = 'z', .c = 'a'};
  base::Traverse(upper, a);
  EXPECT_EQ(a, (Aggregate<char>{.n = 'Z', .c = 'A'}));
}

TEST(Primitives, Containers) {
  static_assert((TraversableBy<std::string, ToUpper>));
  static_assert((TraversableBy<std::vector<std::string>, ToUpper>));
  static_assert((TraversableBy<std::list<std::vector<std::string>>, ToUpper>));

  ToUpper upper;
  std::string s = "abc";
  base::Traverse(upper, s);
  EXPECT_EQ(s , "ABCx");
  std::list<std::vector<std::string>> l;
  l.emplace_back() = {"abc", "def", "ghi"};
  l.emplace_back() = {"jkl", "mno"};
  l.emplace_back() = {};
  l.emplace_back() = {"pqrstuvwxyz"};
  base::Traverse(upper, l);
  EXPECT_THAT(l, ElementsAre(ElementsAre("ABCx", "DEFx", "GHIx"),
                             ElementsAre("JKLx", "MNOx"), ElementsAre(),
                             ElementsAre("PQRSTUVWXYZx")));
}

}  // namespace
}  // namespace base
