#include "base/meta.h"
#include "test/catch.h"

#include <type_traits>

namespace base {
namespace {

TEST_CASE("first") {
  CHECK(std::is_same_v<first_t<int>, int>);
  CHECK(std::is_same_v<first_t<int, int>, int>);
  CHECK(std::is_same_v<first_t<int, bool>, int>);
  CHECK(std::is_same_v<first_t<bool, int>, bool>);
}

TEST_CASE("identity") {
  CHECK(std::is_same_v<identity_t<int>, int>);
  CHECK(std::is_same_v<identity_t<bool>, bool>);
}

TEST_CASE("Type lists") {
  CHECK(std::is_same_v<type_list<>, type_list<>>);
  CHECK(not std::is_same_v<type_list<int>, type_list<>>);
  CHECK(not std::is_same_v<type_list<int>, type_list<int, int>>);
  CHECK(not std::is_same_v<type_list<int, bool>, type_list<bool, int>>);

  using MyInt = int;
  CHECK(std::is_same_v<type_list<MyInt>, type_list<int>>);
}

TEST_CASE("Type lists concatenation") {
  CHECK(std::is_same_v<type_list_cat<>, type_list<>>);
  CHECK(std::is_same_v<type_list_cat<type_list<>>, type_list<>>);
  CHECK(std::is_same_v<type_list_cat<type_list<int, bool>>,
                       type_list<int, bool>>);
  CHECK(std::is_same_v<type_list_cat<type_list<>, type_list<>>, type_list<>>);
  CHECK(std::is_same_v<type_list_cat<type_list<int>, type_list<>>,
                       type_list<int>>);
  CHECK(std::is_same_v<type_list_cat<type_list<>, type_list<int>>,
                       type_list<int>>);
  CHECK(std::is_same_v<type_list_cat<type_list<bool>, type_list<int>>,
                       type_list<bool, int>>);
  CHECK(std::is_same_v<
        type_list_cat<type_list<bool>, type_list<int, char>, type_list<int>>,
        type_list<bool, int, char, int>>);
}

}  // namespace
}  // namespace base
