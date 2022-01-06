#include "base/meta.h"

#include <type_traits>
#include <vector>
#include <array>

#include "absl/types/span.h"
#include "gtest/gtest.h"
#include "gmock/gmock.h"

namespace base {
namespace {

using ::testing::ElementsAre;

TEST(Meta, First) {
  EXPECT_TRUE((std::is_same_v<first_t<int>, int>));
  EXPECT_TRUE((std::is_same_v<first_t<int, int>, int>));
  EXPECT_TRUE((std::is_same_v<first_t<int, bool>, int>));
  EXPECT_TRUE((std::is_same_v<first_t<bool, int>, bool>));
}

TEST(Meta, Identity) {
  EXPECT_TRUE((std::is_same_v<identity_t<int>, int>));
  EXPECT_TRUE((std::is_same_v<identity_t<bool>, bool>));
}

TEST(Meta, TypeList) {
  EXPECT_TRUE((std::is_same_v<type_list<>, type_list<>>));
  EXPECT_TRUE(not(std::is_same_v<type_list<int>, type_list<>>));
  EXPECT_TRUE(not(std::is_same_v<type_list<int>, type_list<int, int>>));
  EXPECT_TRUE(not(std::is_same_v<type_list<int, bool>, type_list<bool, int>>));

  using MyInt = int;
  EXPECT_TRUE((std::is_same_v<type_list<MyInt>, type_list<int>>));
}

TEST(Meta, TypeListConcatenation) {
  EXPECT_TRUE((std::is_same_v<type_list_cat<>, type_list<>>));
  EXPECT_TRUE((std::is_same_v<type_list_cat<type_list<>>, type_list<>>));
  EXPECT_TRUE((std::is_same_v<type_list_cat<type_list<int, bool>>,
                              type_list<int, bool>>));
  EXPECT_TRUE(
      (std::is_same_v<type_list_cat<type_list<>, type_list<>>, type_list<>>));
  EXPECT_TRUE((std::is_same_v<type_list_cat<type_list<int>, type_list<>>,
                              type_list<int>>));
  EXPECT_TRUE((std::is_same_v<type_list_cat<type_list<>, type_list<int>>,
                              type_list<int>>));
  EXPECT_TRUE((std::is_same_v<type_list_cat<type_list<bool>, type_list<int>>,
                              type_list<bool, int>>));
  EXPECT_TRUE(
      (std::is_same_v<
          type_list_cat<type_list<bool>, type_list<int, char>, type_list<int>>,
          type_list<bool, int, char, int>>));
}

TEST(Meta, Comparison) {
  EXPECT_EQ(meta<int>, meta<int>);
  EXPECT_NE(meta<int>, meta<bool>);
}

TEST(Meta, Container) {
  struct S {
    void begin() {}
    void end() {}
  };
  EXPECT_TRUE(Container<std::vector<int>>);
  EXPECT_TRUE(Container<absl::Span<int>>);
  EXPECT_TRUE(Container<absl::Span<int const>>);
  EXPECT_FALSE(Container<S>);
}

TEST(Meta, Index) {
  EXPECT_EQ(-1, Index<int>(type_list<>{}));
  EXPECT_EQ(0, Index<int>(type_list<int, bool>{}));
  EXPECT_EQ(1, Index<bool>(type_list<int, bool>{}));
  EXPECT_EQ(-1, Index<char>(type_list<int, bool>{}));
  EXPECT_EQ(2, Index<int>(type_list<char, bool, int, bool>{}));
  EXPECT_EQ(1, Index<bool>(type_list<char, bool, int, bool>{}));
}

template <typename T>
struct Size {
  static constexpr size_t value = sizeof(T);
};

TEST(Meta, ArrayTransform) {
  EXPECT_THAT((array_transform<Size, type_list<>>), ElementsAre());
  EXPECT_THAT((array_transform<Size, type_list<int>>), ElementsAre(sizeof(int)));
  EXPECT_THAT((array_transform<Size, type_list<int, bool>>),
              ElementsAre(sizeof(int), sizeof(bool)));
}

}  // namespace
}  // namespace base
