#include "base/meta.h"

#include <array>
#include <type_traits>
#include <vector>

#include "absl/types/span.h"
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
  EXPECT_THAT((array_transform<Size, type_list<int>>),
              ElementsAre(sizeof(int)));
  EXPECT_THAT((array_transform<Size, type_list<int, bool>>),
              ElementsAre(sizeof(int), sizeof(bool)));
}

TEST(Meta, GetEntriesImpl) {
  std::tuple t(0, 1, 4, 9, 16, 25);
  EXPECT_EQ((internal_meta::GetEntriesImpl<std::index_sequence<>>{}(t)),
            std::tuple());
  EXPECT_EQ((internal_meta::GetEntriesImpl<std::index_sequence<3>>{}(t)),
            std::tuple(9));
  EXPECT_EQ((internal_meta::GetEntriesImpl<std::index_sequence<3, 5>>{}(t)),
            std::tuple(9, 25));
  EXPECT_EQ((internal_meta::GetEntriesImpl<std::index_sequence<5, 3>>{}(t)),
            std::tuple(25, 9));
  EXPECT_EQ((internal_meta::GetEntriesImpl<std::index_sequence<2, 5, 2>>{}(t)),
            std::tuple(4, 25, 4));
}

TEST(Meta, AddImpl) {
  EXPECT_EQ(
      (base::meta<
          typename internal_meta::AddImpl<3, std::index_sequence<>>::type>),
      (base::meta<std::index_sequence<>>));
  EXPECT_EQ(
      (base::meta<
          typename internal_meta::AddImpl<3, std::index_sequence<1>>::type>),
      (base::meta<std::index_sequence<4>>));
  EXPECT_EQ((base::meta<typename internal_meta::AddImpl<
                 5, std::index_sequence<1, 2, 3>>::type>),
            (base::meta<std::index_sequence<6, 7, 8>>));
}

template <typename I, typename T>
using Wrap = typename internal_meta::AddTupleImpl<I, T>::type;

TEST(Meta, AddTupleImpl) {
  EXPECT_EQ((base::meta<Wrap<std::index_sequence<>, std::tuple<>>>),
            (base::meta<std::tuple<>>));
  EXPECT_EQ((base::meta<Wrap<std::index_sequence<3>,
                             std::tuple<std::index_sequence<1, 2, 3>>>>),
            (base::meta<std::tuple<std::index_sequence<4, 5, 6>>>));

  EXPECT_EQ((base::meta<Wrap<std::index_sequence<3, 2>,
                             std::tuple<std::index_sequence<1, 2, 3>,
                                        std::index_sequence<10, 20>>>>),
            (base::meta<std::tuple<std::index_sequence<4, 5, 6>,
                                   std::index_sequence<12, 22>>>));
}

TEST(Meta, PartialSum) {
  EXPECT_EQ(
      (base::meta<
          typename internal_meta::PartialSum<std::index_sequence<>>::type>),
      (base::meta<std::index_sequence<>>));
  EXPECT_EQ(
      (base::meta<
          typename internal_meta::PartialSum<std::index_sequence<1>>::type>),
      (base::meta<std::index_sequence<0>>));
  EXPECT_EQ(
      (base::meta<
          typename internal_meta::PartialSum<std::index_sequence<2>>::type>),
      (base::meta<std::index_sequence<0>>));
  EXPECT_EQ(
      (base::meta<
          typename internal_meta::PartialSum<std::index_sequence<2, 3>>::type>),
      (base::meta<std::index_sequence<0, 2>>));
  EXPECT_EQ((base::meta<typename internal_meta::PartialSum<
                 std::index_sequence<2, 3, 4, 5>>::type>),
            (base::meta<std::index_sequence<0, 2, 5, 9>>));
}

TEST(Meta, SplitTuple) {
  std::tuple t( 1, 2, 3);
  EXPECT_EQ(SplitTuple<3>(t),
            (std::tuple<std::tuple<int, int, int>>(std::tuple(1, 2, 3))));
  EXPECT_EQ((SplitTuple<1, 2>(t)),
            (std::tuple<std::tuple<int>, std::tuple<int, int>>(
                std::tuple(1), std::tuple(2, 3))));
  EXPECT_EQ((SplitTuple<2, 1>(t)),
            (std::tuple<std::tuple<int, int>, std::tuple<int>>(std::tuple(1, 2),
                                                               std::tuple(3))));
  EXPECT_EQ((SplitTuple<1, 1, 1>(t)),
            (std::tuple<std::tuple<int>, std::tuple<int>, std::tuple<int>>(
                std::tuple(1), std::tuple(2), std::tuple(3))));
}

}  // namespace
}  // namespace base
