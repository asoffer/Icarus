#include "base/flyweight_map.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace base {
namespace {

using ::testing::ElementsAre;
using ::testing::Pair;

MATCHER_P(IteratorRefersTo, matcher, "") {
  return ::testing::ExplainMatchResult(matcher, *arg, result_listener);
}

TEST(FlyweightMap, DefaultConstruction) {
  flyweight_map<int, std::string> f;
  EXPECT_TRUE(f.empty());
  EXPECT_EQ(f.size(), 0);
}

TEST(FlyweightMap, TryEmplace) {
  flyweight_map<int, std::string> f;

  auto [p1, inserted1] = f.try_emplace(1, 1, 'x');
  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 1);

  auto [p2, inserted2] = f.try_emplace(2, 1, 'y');
  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 2);

  auto [p3, inserted3] = f.try_emplace(1, 1, 'z');
  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 2);

  EXPECT_TRUE(inserted1);
  EXPECT_TRUE(inserted2);
  EXPECT_FALSE(inserted3);

  EXPECT_NE(p1, p2);
  EXPECT_EQ(p1, p3);

  EXPECT_THAT(f, ElementsAre(Pair(1, "x"), Pair(2, "y")));
}

TEST(FlyweightMap, BracketOperator) {
  flyweight_map<int, std::string> f;

  f[1];
  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 1);
  EXPECT_THAT(f.find(1), IteratorRefersTo(Pair(1, "")));

  f[1] = "a";
  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 1);
  EXPECT_THAT(f.find(1), IteratorRefersTo(Pair(1, "a")));

  f[2] = "b";
  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 2);
  EXPECT_THAT(f.find(2), IteratorRefersTo(Pair(2, "b")));

  f[1] = "c";
  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 2);
  EXPECT_THAT(f.find(1), IteratorRefersTo(Pair(1, "c")));

  EXPECT_THAT(f, ElementsAre(Pair(1, "c"), Pair(2, "b")));
}

TEST(FlyweightMap, ListInitialization) {
  flyweight_map<int, std::string> f{{3, "a"}, {2, "b"}, {1, "c"},
                                    {2, "d"}, {3, "e"}, {4, "f"}};

  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 4);

  EXPECT_THAT(
      f, ElementsAre(Pair(3, "a"), Pair(2, "b"), Pair(1, "c"), Pair(4, "f")));
}

TEST(FlyweightMap, Clear) {
  flyweight_map<int, std::string> f{{1, "a"}, {2, "b"}, {3, "c"}, {4, "d"}};
  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 4);
  f.clear();
  EXPECT_TRUE(f.empty());
  EXPECT_EQ(f.size(), 0);
}

TEST(FlyweightMap, CopyConstruction) {
  flyweight_map<int, std::string> f{{1, "a"}, {2, "b"}, {3, "c"}, {4, "d"}};
  flyweight_map<int, std::string> g = f;

  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 4);

  EXPECT_FALSE(g.empty());
  EXPECT_EQ(g.size(), 4);

  EXPECT_THAT(
      f, ElementsAre(Pair(1, "a"), Pair(2, "b"), Pair(3, "c"), Pair(4, "d")));
  EXPECT_THAT(
      g, ElementsAre(Pair(1, "a"), Pair(2, "b"), Pair(3, "c"), Pair(4, "d")));

  // Ensure that the members of `g` are not referencing into `f`
  f[1] = "A";
  EXPECT_THAT(g.find(1), IteratorRefersTo(Pair(1, "a")));
}

TEST(FlyweightMap, MoveConstruction) {
  flyweight_map<int, std::string> f{{1, "a"}, {2, "b"}, {3, "c"}, {4, "d"}};
  flyweight_map<int, std::string> g = std::move(f);

  EXPECT_FALSE(g.empty());
  EXPECT_EQ(g.size(), 4);
  EXPECT_THAT(
      g, ElementsAre(Pair(1, "a"), Pair(2, "b"), Pair(3, "c"), Pair(4, "d")));
}

TEST(FlyweightMap, CopyAssignment) {
  flyweight_map<int, std::string> f{{1, "a"}, {2, "b"}, {3, "c"}, {4, "d"}};
  flyweight_map<int, std::string> g;
  g = f;

  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 4);

  EXPECT_FALSE(g.empty());
  EXPECT_EQ(g.size(), 4);

  EXPECT_THAT(
      f, ElementsAre(Pair(1, "a"), Pair(2, "b"), Pair(3, "c"), Pair(4, "d")));
  EXPECT_THAT(
      g, ElementsAre(Pair(1, "a"), Pair(2, "b"), Pair(3, "c"), Pair(4, "d")));

  f[1] = "A";
  EXPECT_THAT(g.find(1), IteratorRefersTo(Pair(1, "a")));
}

TEST(FlyweightMap, MoveAssignment) {
  flyweight_map<int, std::string> f{{1, "a"}, {2, "b"}, {3, "c"}, {4, "d"}};
  flyweight_map<int, std::string> g;
  g = std::move(f);

  EXPECT_FALSE(g.empty());
  EXPECT_EQ(g.size(), 4);
  EXPECT_THAT(
      g, ElementsAre(Pair(1, "a"), Pair(2, "b"), Pair(3, "c"), Pair(4, "d")));
}

TEST(FlyweightMap, Iterators) {
  flyweight_map<int, std::string> f{{1, "a"}, {2, "b"}, {3, "c"},
                                    {4, "d"}, {1, "e"}, {2, "f"}};
  EXPECT_THAT(
      std::vector(f.begin(), f.end()),
      ElementsAre(Pair(1, "a"), Pair(2, "b"), Pair(3, "c"), Pair(4, "d")));
  EXPECT_THAT(
      std::vector(f.cbegin(), f.cend()),
      ElementsAre(Pair(1, "a"), Pair(2, "b"), Pair(3, "c"), Pair(4, "d")));
  EXPECT_THAT(
      std::vector(f.rbegin(), f.rend()),
      ElementsAre(Pair(4, "d"), Pair(3, "c"), Pair(2, "b"), Pair(1, "a")));
  EXPECT_THAT(
      std::vector(f.crbegin(), f.crend()),
      ElementsAre(Pair(4, "d"), Pair(3, "c"), Pair(2, "b"), Pair(1, "a")));
}

TEST(FlyweightMap, ConstAccess) {
  flyweight_map<int, std::string> const f{
      {1, "a"}, {2, "b"}, {1, "c"}, {3, "d"}, {1, "e"}};
  EXPECT_THAT(f.front(), Pair(1, "a"));
  EXPECT_THAT(f.back(), Pair(3, "d"));
}

TEST(FlyweightMap, Access) {
  flyweight_map<int, std::string> f{
      {1, "a"}, {2, "b"}, {1, "c"}, {3, "d"}, {1, "e"}};
  EXPECT_THAT(f.front(), Pair(1, "a"));
  EXPECT_THAT(f.back(), Pair(3, "d"));

  f.front().second = "A";
  f.back().second  = "D";
  EXPECT_THAT(f.front(), Pair(1, "A"));
  EXPECT_THAT(f.back(), Pair(3, "D"));
}

TEST(FlyweightMap, Find) {
  flyweight_map<int, std::string> f{{1, "a"}, {2, "b"}, {3, "c"}, {4, "d"}};
  EXPECT_EQ(f.find(0), f.end());
  EXPECT_EQ(f.find(1), f.begin());
  EXPECT_THAT(*f.find(4), Pair(4, "d"));
  ASSERT_NE(f.find(4), f.end());
  EXPECT_EQ(std::distance(f.begin(), f.find(4)), 3);
}

TEST(FlyweightMap, StressTest) {
  flyweight_map<size_t, size_t> f;
  for (size_t i = 0; i < 1000; ++i) {
    f.try_emplace(i);
    for (auto [j, ignored] : f) { ASSERT_EQ(f.index(j) , j) << i; }
  }
}

}  // namespace
}  // namespace base
