#include "base/meta.h"

#include <type_traits>

#include "gtest/gtest.h"

namespace base {
namespace {

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

}  // namespace
}  // namespace base
