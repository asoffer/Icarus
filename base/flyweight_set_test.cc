#include "base/flyweight_set.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace base {
namespace {

using ::testing::ElementsAre;
using ::testing::Pair;
using ::testing::Pointee;

TEST(FlyweightSet, DefaultConstruction) {
  flyweight_set<std::string> f;
  EXPECT_TRUE(f.empty());
  EXPECT_EQ(f.size(), 0);
}

TEST(FlyweightSet, Insert) {
  flyweight_set<std::string> f;

  auto [p1, inserted1] = f.insert("abc");
  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 1);

  auto [p2, inserted2] = f.insert("def");
  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 2);

  auto [p3, inserted3] = f.insert("abc");
  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 2);

  EXPECT_TRUE(inserted1);
  EXPECT_TRUE(inserted2);
  EXPECT_FALSE(inserted3);

  EXPECT_NE(p1, p2);
  EXPECT_EQ(p1, p3);

  EXPECT_THAT(f, ElementsAre("abc", "def"));
}

TEST(FlyweightSet, ListInitialization) {
  flyweight_set<std::string> f{"a", "b", "c", "a", "c", "f"};

  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 4);

  EXPECT_THAT(f, ElementsAre("a", "b", "c", "f"));
}

TEST(FlyweightSet, Clear) {
  flyweight_set<std::string> f{"a", "b", "c", "d"};
  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 4);
  f.clear();
  EXPECT_TRUE(f.empty());
  EXPECT_EQ(f.size(), 0);
}

TEST(FlyweightSet, CopyConstruction) {
  flyweight_set<std::string> f{"a", "b", "c", "d"};
  flyweight_set<std::string> g = f;

  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 4);

  EXPECT_FALSE(g.empty());
  EXPECT_EQ(g.size(), 4);

  EXPECT_THAT(f, ElementsAre("a", "b", "c", "d"));
  EXPECT_THAT(g, ElementsAre("a", "b", "c", "d"));
}

TEST(FlyweightSet, MoveConstruction) {
  flyweight_set<std::string> f{"a", "b", "c", "d"};
  flyweight_set<std::string> g = std::move(f);

  EXPECT_FALSE(g.empty());
  EXPECT_EQ(g.size(), 4);
  EXPECT_THAT(g, ElementsAre("a", "b", "c", "d"));
}

TEST(FlyweightSet, CopyAssignment) {
  flyweight_set<std::string> f{"a", "b", "c", "d"};
  flyweight_set<std::string> g;
  g = f;

  EXPECT_FALSE(f.empty());
  EXPECT_EQ(f.size(), 4);

  EXPECT_FALSE(g.empty());
  EXPECT_EQ(g.size(), 4);

  EXPECT_THAT(f, ElementsAre("a", "b", "c", "d"));
  EXPECT_THAT(g, ElementsAre("a", "b", "c", "d"));
}

TEST(FlyweightSet, MoveAssignment) {
  flyweight_set<std::string> f{"a", "b", "c", "d"};
  flyweight_set<std::string> g;
  g = std::move(f);

  EXPECT_FALSE(g.empty());
  EXPECT_EQ(g.size(), 4);
  EXPECT_THAT(g, ElementsAre("a", "b", "c", "d"));
}

TEST(FlyweightSet, Iterators) {
  flyweight_set<std::string> f{"a", "b", "c", "d", "c", "b"};
  EXPECT_THAT(std::vector(f.begin(), f.end()), ElementsAre("a", "b", "c", "d"));
  EXPECT_THAT(std::vector(f.cbegin(), f.cend()),
              ElementsAre("a", "b", "c", "d"));
  EXPECT_THAT(std::vector(f.rbegin(), f.rend()),
              ElementsAre("d", "c", "b", "a"));
  EXPECT_THAT(std::vector(f.crbegin(), f.crend()),
              ElementsAre("d", "c", "b", "a"));
}

TEST(FlyweightSet, ConstAccess) {
  flyweight_set<std::string> const f{"a", "b", "c", "d", "a"};
  EXPECT_THAT(f.front(), "a");
  EXPECT_THAT(f.back(), "d");
}

TEST(FlyweightSet, Find) {
  flyweight_set<std::string> f{"a", "b", "c", "d"};
  EXPECT_EQ(f.find("x"), f.end());
  EXPECT_EQ(f.find("a"), f.begin());
  EXPECT_EQ(f.find("d"), std::prev(f.end()));
}

TEST(FlyweightSet, Index) {
  flyweight_set<std::string> f{"a", "b", "c", "d"};
  EXPECT_EQ(f.index("x"), f.end_index());
  EXPECT_EQ(f.index("a"), 0);
  EXPECT_EQ(f.index("d"), 3);
}

}  // namespace
}  // namespace base

