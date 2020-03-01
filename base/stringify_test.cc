#include "base/stringify.h"

#include <cmath>
#include <limits>
#include <map>
#include <set>
#include <vector>

#include "gtest/gtest.h"

namespace base {
namespace {

TEST(Stringify, Strings) {
  EXPECT_EQ(stringify(std::string{"hello"}), "hello");
  EXPECT_EQ(stringify(std::string{""}), "");

  EXPECT_EQ(stringify("hello"), "hello");
  EXPECT_EQ(stringify(""), "");
}

TEST(Stringify, Pointers) {
  EXPECT_EQ(stringify(nullptr), "nullptr");
  EXPECT_EQ(stringify(static_cast<char const*>(nullptr)), "null char const*");

  char const* chars = "hello";
  EXPECT_EQ(stringify(chars), "hello");

  int n;
  EXPECT_NE(stringify(&n), "0x0000000000000000");
  EXPECT_EQ(stringify(static_cast<int*>(nullptr)), "0x0000000000000000");
}

TEST(Stringify, Bools) {
  EXPECT_EQ(stringify(true), "true");
  EXPECT_EQ(stringify(false), "false");
}

TEST(Stringify, Ints) {
  EXPECT_EQ(stringify(0), "0");
  EXPECT_EQ(stringify(-3), "-3");
  EXPECT_EQ(stringify(17), "17");

  EXPECT_EQ(stringify(0ll), "0");
  EXPECT_EQ(stringify(-3ll), "-3");
  EXPECT_EQ(stringify(17ll), "17");
  EXPECT_EQ(stringify(std::numeric_limits<int64_t>::max()),
            "9223372036854775807");

  EXPECT_EQ(stringify(0ull), "0");
  EXPECT_EQ(stringify(17ull), "17");
  EXPECT_EQ(stringify(std::numeric_limits<uint64_t>::max()),
            "18446744073709551615");
}

TEST(Stringify, Chars) {
  EXPECT_EQ(stringify('x'), "x");
  EXPECT_EQ(stringify('\n'), "\n");
  EXPECT_EQ(stringify('\0'), "");
}

TEST(Stringify, Floats) {
  EXPECT_EQ(stringify(0.0f), "0.000000");
  EXPECT_EQ(stringify(-0.0f), "-0.000000");
  EXPECT_EQ(stringify(std::nanf("tag")), "nan");
  EXPECT_EQ(stringify(17.34f), "17.340000");

  EXPECT_EQ(stringify(0.0), "0.000000");
  EXPECT_EQ(stringify(-0.0), "-0.000000");
  EXPECT_EQ(stringify(std::nan("tag")), "nan");
  EXPECT_EQ(stringify(17.34), "17.340000");
}

TEST(Stringify, Pairs) {
  EXPECT_EQ(stringify(std::pair(3, 4)), "(3, 4)");
  EXPECT_EQ(stringify(std::pair(3, std::pair(true, "hello"))),
            "(3, (true, hello))");
}

TEST(Stringify, Tuples) {
  EXPECT_EQ(stringify(std::tuple<>()), "()");
  EXPECT_EQ(stringify(std::tuple(false)), "(false)");
  EXPECT_EQ(stringify(std::tuple(3, 4)), "(3, 4)");
  EXPECT_EQ(stringify(std::tuple(true, 3, std::pair(false, "hello"))),
            "(true, 3, (false, hello))");
}

TEST(Stringify, Variants) {
  std::variant<int, bool> v = 3;
  EXPECT_EQ(stringify(v), "3");
  v = true;
  EXPECT_EQ(stringify(v), "true");

  std::variant<int, bool, std::variant<int, bool>> w = 3;
  EXPECT_EQ(stringify(w), "3");
  w = true;
  EXPECT_EQ(stringify(v), "true");
  w = v;
  EXPECT_EQ(stringify(w), "true");
}

TEST(Stringify, Optional) {
  std::optional<int> n;
  EXPECT_EQ(stringify(n), "nullopt");
  n = 3;
  EXPECT_EQ(stringify(n), "3");
}

TEST(Stringify, Containers) {
  EXPECT_EQ(stringify(std::vector<int>{}), "[]");
  EXPECT_EQ(stringify(std::vector<int>{17}), "[17]");

  EXPECT_EQ(stringify(std::vector<int>{1, 2, 3, 4}), "[1, 2, 3, 4]");
  EXPECT_EQ(stringify(std::map<int, std::string>{{2, "hello"}, {5, "world"}}),
            "[(2, hello), (5, world)]");
  EXPECT_EQ(stringify(std::set<int>{8, 4, 6, 2}), "[2, 4, 6, 8]");
}

TEST(Stringify, HasToStringMethod) {
  struct S {
    std::string to_string() const { return "to_string!"; }
  };
  EXPECT_EQ(stringify(S{}), "to_string!");
  EXPECT_EQ(stringify(std::pair{S{}, S{}}), "(to_string!, to_string!)");
}

struct Streamable {};
std::ostream& operator<<(std::ostream& os, Streamable) {
  return os << "streamable!";
}

TEST(Stringify, Streamable) {
  EXPECT_EQ(stringify(Streamable{}), "streamable!");
}

}  // namespace
}  // namespace base

struct S {};
std::string stringify(S const&) { return "adl"; }

TEST(Stringify, ADL) {
  using base::stringify;
  EXPECT_EQ(stringify(S{}), "adl");
  EXPECT_EQ(stringify(std::pair{S{}, S{}}), "(adl, adl)");
}
