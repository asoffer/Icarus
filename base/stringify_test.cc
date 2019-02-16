#include "base/stringify.h"

#include <cmath>
#include <limits>
#include <map>
#include <set>
#include <vector>

#include "test/test.h"

namespace base {
namespace {
using ::test::Eq;

TEST(StringifyStrings) {
  CHECK(stringify(std::string{"hello"}), Eq("hello"));
  CHECK(stringify(std::string{""}), Eq(""));

  CHECK(stringify("hello"), Eq("hello"));
  CHECK(stringify(""), Eq(""));
}

TEST(StringifyBool) {
  CHECK(stringify(true), Eq("true"));
  CHECK(stringify(false), Eq("false"));
}

TEST(StringifyIntegers) {
  CHECK(stringify(0), Eq("0"));
  CHECK(stringify(-3), Eq("-3"));
  CHECK(stringify(17), Eq("17"));

  CHECK(stringify(0ll), Eq("0"));
  CHECK(stringify(-3ll), Eq("-3"));
  CHECK(stringify(17ll), Eq("17"));
  CHECK(stringify(std::numeric_limits<int64_t>::max()),
        Eq("9223372036854775807"));

  CHECK(stringify(0ull), Eq("0"));
  CHECK(stringify(17ull), Eq("17"));
  CHECK(stringify(std::numeric_limits<uint64_t>::max()),
        Eq("18446744073709551615"));
}

TEST(StringifyChars) {
  CHECK(stringify('x'), Eq("x"));
  CHECK(stringify('\n'), Eq("\n"));
  CHECK(stringify('\0'), Eq(""));
}

TEST(StringifyFloats) {
  CHECK(stringify(0.0f), Eq("0.000000"));
  CHECK(stringify(-0.0f), Eq("-0.000000"));
  CHECK(stringify(std::nanf("tag")), Eq("nan"));
  CHECK(stringify(17.34f), Eq("17.340000"));

  CHECK(stringify(0.0), Eq("0.000000"));
  CHECK(stringify(-0.0), Eq("-0.000000"));
  CHECK(stringify(std::nan("tag")), Eq("nan"));
  CHECK(stringify(17.34), Eq("17.340000"));
}

TEST(StringifyPair) {
  CHECK(stringify(std::pair(3, 4)), Eq("(3, 4)"));
  CHECK(stringify(std::pair(3, std::pair(true, "hello"))),
        Eq("(3, (true, hello))"));
}

TEST(StringifyTuple) {
  CHECK(stringify(std::tuple<>()), Eq("()"));
  CHECK(stringify(std::tuple(false)), Eq("(false)"));
  CHECK(stringify(std::tuple(3, 4)), Eq("(3, 4)"));
  CHECK(stringify(std::tuple(true, 3, std::pair(false, "hello"))),
        Eq("(true, 3, (false, hello))"));
}

TEST(StringifyVariant) {
  std::variant<int, bool> v = 3;
  CHECK(stringify(v), Eq("3"));
  v = true;
  CHECK(stringify(v), Eq("true"));

  std::variant<int, bool, std::variant<int, bool>> w = 3;
  CHECK(stringify(w), Eq("3"));
  w = true;
  CHECK(stringify(v), Eq("true"));
  w = v;
  CHECK(stringify(w), Eq("true"));
}

TEST(StringifyOptional) {
  std::optional<int> n;
  CHECK(stringify(n), Eq("nullopt"));
  n = 3;
  CHECK(stringify(n), Eq("3"));
}

TEST(StringifyContainer) {
  CHECK(stringify(std::vector<int>{}), Eq("[]"));
  CHECK(stringify(std::vector<int>{17}), Eq("[17]"));

  CHECK(stringify(std::vector<int>{1, 2, 3, 4}), Eq("[1, 2, 3, 4]"));
  CHECK(stringify(std::map<int, std::string>{{2, "hello"}, {5, "world"}}),
        Eq("[(2, hello), (5, world)]"));
  CHECK(stringify(std::set<int>{8, 4, 6, 2}), Eq("[2, 4, 6, 8]"));
}

TEST(StringifyWithToStringMethod) {
  struct S {
    std::string to_string() const { return "to_string!"; }
  };
  CHECK(stringify(S{}), Eq("to_string!"));
  CHECK(stringify(std::pair{S{}, S{}}), Eq("(to_string!, to_string!)"));
}

}  // namespace
}  // namespace base

struct S {};
std::string stringify(S const&) { return "adl"; }

TEST(StringifyAdlOverload) {
  using base::stringify;
  CHECK(stringify(S{}), test::Eq("adl"));
  CHECK(stringify(std::pair{S{}, S{}}), test::Eq("(adl, adl)"));
}
