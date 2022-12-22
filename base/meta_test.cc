#include "base/meta.h"

#include <array>
#include <span>
#include <type_traits>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

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
  EXPECT_TRUE(Container<std::span<int>>);
  EXPECT_TRUE(Container<std::span<int const>>);
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
  EXPECT_THAT((array_transform<Size, type_list<int>>),
              ElementsAre(sizeof(int)));
  EXPECT_THAT((array_transform<Size, type_list<int, bool>>),
              ElementsAre(sizeof(int), sizeof(bool)));
}

template <typename>
struct True {
  static constexpr bool value = true;
};
template <typename>
struct False {
  static constexpr bool value = false;
};
template <typename T>
struct OneByte {
  static constexpr bool value = sizeof(T) == 1;
};

TEST(Meta, AllOf) {
  EXPECT_TRUE((all_of<type_list<>, True>));
  EXPECT_TRUE((all_of<type_list<>, False>));

  EXPECT_TRUE((all_of<type_list<int>, True>));
  EXPECT_FALSE((all_of<type_list<int>, False>));

  EXPECT_TRUE((all_of<type_list<>, OneByte>));
  EXPECT_TRUE((all_of<type_list<std::byte>, OneByte>));
  EXPECT_TRUE((all_of<type_list<std::byte, char>, OneByte>));
  EXPECT_FALSE((all_of<type_list<std::byte, int, char>, OneByte>));
}

TEST(Meta, Filter) {
  EXPECT_EQ((meta<filter<type_list<>, True>>), meta<type_list<>>);
  EXPECT_EQ((meta<filter<type_list<int>, True>>), meta<type_list<int>>);
  EXPECT_EQ((meta<filter<type_list<int, bool>, True>>),
            (meta<type_list<int, bool>>));

  EXPECT_EQ((meta<filter<type_list<>, False>>), meta<type_list<>>);
  EXPECT_EQ((meta<filter<type_list<int>, False>>), meta<type_list<>>);
  EXPECT_EQ((meta<filter<type_list<int, bool>, False>>), meta<type_list<>>);

  EXPECT_EQ((meta<filter<type_list<>, OneByte>>), meta<type_list<>>);
  EXPECT_EQ((meta<filter<type_list<int>, OneByte>>), meta<type_list<>>);
  EXPECT_EQ((meta<filter<type_list<int, char, float, std::byte>, OneByte>>),
            (meta<type_list<char, std::byte>>));
}

}  // namespace
}  // namespace base
