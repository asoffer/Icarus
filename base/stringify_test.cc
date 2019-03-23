#include "base/stringify.h"

#include <cmath>
#include <limits>
#include <map>
#include <set>
#include <vector>

#include "test/catch.h"

namespace base {
namespace {

TEST_CASE("strings") {
  CHECK(stringify(std::string{"hello"}) == "hello");
  CHECK(stringify(std::string{""}) == "");

  CHECK(stringify("hello") == "hello");
  CHECK(stringify("") == "");
}

TEST_CASE("pointers") {
  CHECK(stringify(nullptr) == "nullptr");
  CHECK(stringify(static_cast<char const*>(nullptr)) == "null char const*");

  char const* chars = "hello";
  CHECK(stringify(chars) == "hello");

  int n;
  CHECK(stringify(&n) != "0x0000000000000000");
  CHECK(stringify(static_cast<int*>(nullptr)) == "0x0000000000000000");
}

TEST_CASE("bools") {
  CHECK(stringify(true) == "true");
  CHECK(stringify(false) == "false");
}

TEST_CASE("ints") {
  CHECK(stringify(0) == "0");
  CHECK(stringify(-3) == "-3");
  CHECK(stringify(17) == "17");

  CHECK(stringify(0ll) == "0");
  CHECK(stringify(-3ll) == "-3");
  CHECK(stringify(17ll) == "17");
  CHECK(stringify(std::numeric_limits<int64_t>::max()) ==
        "9223372036854775807");

  CHECK(stringify(0ull) == "0");
  CHECK(stringify(17ull) == "17");
  CHECK(stringify(std::numeric_limits<uint64_t>::max()) ==
        "18446744073709551615");
}

TEST_CASE("chars") {
  CHECK(stringify('x') == "x");
  CHECK(stringify('\n') == "\n");
  CHECK(stringify('\0') == "");
}

TEST_CASE("floats") {
  CHECK(stringify(0.0f) == "0.000000");
  CHECK(stringify(-0.0f) == "-0.000000");
  CHECK(stringify(std::nanf("tag")) == "nan");
  CHECK(stringify(17.34f) == "17.340000");

  CHECK(stringify(0.0) == "0.000000");
  CHECK(stringify(-0.0) == "-0.000000");
  CHECK(stringify(std::nan("tag")) == "nan");
  CHECK(stringify(17.34) == "17.340000");
}

TEST_CASE("pairs") {
  CHECK(stringify(std::pair(3, 4)) == "(3, 4)");
  CHECK(stringify(std::pair(3, std::pair(true, "hello"))) ==
        "(3, (true, hello))");
}

TEST_CASE("tuples") {
  CHECK(stringify(std::tuple<>()) == "()");
  CHECK(stringify(std::tuple(false)) == "(false)");
  CHECK(stringify(std::tuple(3, 4)) == "(3, 4)");
  CHECK(stringify(std::tuple(true, 3, std::pair(false, "hello"))) ==
            "(true, 3, (false, hello))");
}

TEST_CASE("variants") {
  std::variant<int, bool> v = 3;
  CHECK(stringify(v) == "3");
  v = true;
  CHECK(stringify(v) == "true");

  std::variant<int, bool, std::variant<int, bool>> w = 3;
  CHECK(stringify(w) == "3");
  w = true;
  CHECK(stringify(v) == "true");
  w = v;
  CHECK(stringify(w) == "true");
}

TEST_CASE("optional") {
  std::optional<int> n;
  CHECK(stringify(n) == "nullopt");
  n = 3;
  CHECK(stringify(n) == "3");
}

TEST_CASE("containers") {
  CHECK(stringify(std::vector<int>{}) == "[]");
  CHECK(stringify(std::vector<int>{17}) == "[17]");

  CHECK(stringify(std::vector<int>{1, 2, 3, 4}) == "[1, 2, 3, 4]");
  CHECK(stringify(std::map<int, std::string>{{2, "hello"}, {5, "world"}}) ==
        "[(2, hello), (5, world)]");
  CHECK(stringify(std::set<int>{8, 4, 6, 2}) == "[2, 4, 6, 8]");
}

TEST_CASE("has to_string method") {
  struct S {
    std::string to_string() const { return "to_string!"; }
  };
  CHECK(stringify(S{}) == "to_string!");
  CHECK(stringify(std::pair{S{}, S{}}) == "(to_string!, to_string!)");
}

struct Streamable {};
std::ostream& operator<<(std::ostream& os, Streamable) {
  return os << "streamable!";
}
TEST_CASE("streamable") {
  CHECK(stringify(Streamable{}) == "streamable!");
}

}  // namespace
}  // namespace base

struct S {};
std::string stringify(S const&) { return "adl"; }

TEST_CASE("ADL overload") {
  using base::stringify;
  CHECK(stringify(S{}) =="adl");
  CHECK(stringify(std::pair{S{}, S{}}) == "(adl, adl)");
}
