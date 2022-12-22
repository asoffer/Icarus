#include "base/universal_print.h"

#include <cmath>
#include <limits>
#include <map>
#include <set>
#include <span>
#include <vector>

#include "gtest/gtest.h"

namespace base {

struct Unprintable {
  int64_t field = 27;
};

enum class Color { Red, Green = 3, Blue };

namespace {

TEST(UniversalPrint, Strings) {
  EXPECT_EQ(UniversalPrintToString(std::string{"hello"}), "hello");
  EXPECT_EQ(UniversalPrintToString(std::string{""}), "");
  EXPECT_EQ(UniversalPrintToString("hello"), "hello");
  EXPECT_EQ(UniversalPrintToString(""), "");
}

TEST(UniversalPrint, Pointers) {
  EXPECT_EQ(UniversalPrintToString(nullptr), "nullptr");
  EXPECT_EQ(UniversalPrintToString(static_cast<char const*>(nullptr)), "");

  char const* chars = "hello";
  EXPECT_EQ(UniversalPrintToString(chars), "hello");

  int n;
  EXPECT_NE(UniversalPrintToString(&n), "0");
  EXPECT_EQ(UniversalPrintToString(static_cast<int*>(nullptr)), "0");
}

TEST(UniversalPrint, Bools) {
  EXPECT_EQ(UniversalPrintToString(true), "true");
  EXPECT_EQ(UniversalPrintToString(false), "false");
}

TEST(UniversalPrint, Ints) {
  EXPECT_EQ(UniversalPrintToString(0), "0");
  EXPECT_EQ(UniversalPrintToString(-3), "-3");
  EXPECT_EQ(UniversalPrintToString(17), "17");

  EXPECT_EQ(UniversalPrintToString(0ll), "0");
  EXPECT_EQ(UniversalPrintToString(-3ll), "-3");
  EXPECT_EQ(UniversalPrintToString(17ll), "17");
  EXPECT_EQ(UniversalPrintToString(std::numeric_limits<int64_t>::max()),
            "9223372036854775807");

  EXPECT_EQ(UniversalPrintToString(0ull), "0");
  EXPECT_EQ(UniversalPrintToString(17ull), "17");
  EXPECT_EQ(UniversalPrintToString(std::numeric_limits<uint64_t>::max()),
            "18446744073709551615");
}

TEST(UniversalPrint, Chars) {
  EXPECT_EQ(UniversalPrintToString('x'), "x");
  EXPECT_EQ(UniversalPrintToString('\n'), "\n");
}

TEST(UniversalPrint, Floats) {
  EXPECT_EQ(UniversalPrintToString(0.0f), "0");
  EXPECT_EQ(UniversalPrintToString(-0.0f), "-0");
  EXPECT_EQ(UniversalPrintToString(std::nanf("tag")), "nan");
  EXPECT_EQ(UniversalPrintToString(17.34f), "17.34");

  EXPECT_EQ(UniversalPrintToString(0.0), "0");
  EXPECT_EQ(UniversalPrintToString(-0.0), "-0");
  EXPECT_EQ(UniversalPrintToString(std::nan("tag")), "nan");
  EXPECT_EQ(UniversalPrintToString(17.34), "17.34");
}

TEST(UniversalPrint, Pairs) {
  EXPECT_EQ(UniversalPrintToString(std::pair(3, 4)), "(3, 4)");
  EXPECT_EQ(UniversalPrintToString(std::pair(3, std::pair(true, "hello"))),
            "(3, (true, hello))");
}

TEST(UniversalPrint, Tuples) {
  EXPECT_EQ(UniversalPrintToString(std::tuple<>()), "()");
  EXPECT_EQ(UniversalPrintToString(std::tuple(false)), "(false)");
  EXPECT_EQ(UniversalPrintToString(std::tuple(3, 4)), "(3, 4)");
  EXPECT_EQ(
      UniversalPrintToString(std::tuple(true, 3, std::pair(false, "hello"))),
      "(true, 3, (false, hello))");
}

TEST(UniversalPrint, Variants) {
  std::variant<int, bool> v = 3;
  EXPECT_EQ(UniversalPrintToString(v), "3");
  v = true;
  EXPECT_EQ(UniversalPrintToString(v), "true");

  std::variant<int, bool, std::variant<int, bool>> w = 3;
  EXPECT_EQ(UniversalPrintToString(w), "3");
  w = true;
  EXPECT_EQ(UniversalPrintToString(v), "true");
  w = v;
  EXPECT_EQ(UniversalPrintToString(w), "true");
}

TEST(UniversalPrint, Optional) {
  std::optional<int> n;
  EXPECT_EQ(UniversalPrintToString(n), "nullopt");
  n = 3;
  EXPECT_EQ(UniversalPrintToString(n), "3");
}

TEST(UniversalPrint, Containers) {
  EXPECT_EQ(UniversalPrintToString(std::vector<int>{}), "[]");
  EXPECT_EQ(UniversalPrintToString(std::vector<int>{17}), "[17]");

  EXPECT_EQ(UniversalPrintToString(std::vector<int>{1, 2, 3, 4}),
            "[1, 2, 3, 4]");
  EXPECT_EQ(UniversalPrintToString(
                std::map<int, std::string>{{2, "hello"}, {5, "world"}}),
            "[(2, hello), (5, world)]");
  EXPECT_EQ(UniversalPrintToString(std::set<int>{8, 4, 6, 2}), "[2, 4, 6, 8]");
}

struct Streamable {};
std::ostream& operator<<(std::ostream& os, Streamable) {
  return os << "streamable!";
}

TEST(UniversalPrint, Streamable) {
  EXPECT_EQ(UniversalPrintToString(Streamable{}), "streamable!");
}

TEST(UniversalPrint, Unprintable) {
  EXPECT_EQ(UniversalPrintToString(Unprintable{}),
            "(Unprintable value of type N4base11UnprintableE)"
            "[1b 00 00 00 00 00 00 00]");
  EXPECT_EQ(UniversalPrintToString(
                std::vector{Unprintable{.field = 1}, Unprintable{.field = 2}}),
            "[(Unprintable value of type N4base11UnprintableE)"
            "[01 00 00 00 00 00 00 00],"
            " (Unprintable value of type N4base11UnprintableE)"
            "[02 00 00 00 00 00 00 00]]");
}

TEST(UniversalPrint, Enum) {
  EXPECT_EQ(UniversalPrintToString(Color::Red), "(N4base5ColorE)0");
  EXPECT_EQ(UniversalPrintToString(Color::Green), "(N4base5ColorE)3");
  EXPECT_EQ(UniversalPrintToString(Color::Blue), "(N4base5ColorE)4");
}

TEST(UniversalPrint, AbslSpan) {
  std::vector v{1, 2, 3, 4};
  std::span<int const> span = v;
  EXPECT_EQ(UniversalPrintToString(span), "[1, 2, 3, 4]");
}

}  // namespace
}  // namespace base
