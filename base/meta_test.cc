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

TEST(Meta, TypeList) {
  EXPECT_TRUE((std::is_same_v<type_list<>, type_list<>>));
  EXPECT_TRUE(not(std::is_same_v<type_list<int>, type_list<>>));
  EXPECT_TRUE(not(std::is_same_v<type_list<int>, type_list<int, int>>));
  EXPECT_TRUE(not(std::is_same_v<type_list<int, bool>, type_list<bool, int>>));

  using MyInt = int;
  EXPECT_TRUE((std::is_same_v<type_list<MyInt>, type_list<int>>));
}

}  // namespace
}  // namespace base
