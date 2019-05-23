#include "match/binding_id.h"

#include "test/catch.h"

namespace match {
namespace {

TEST_CASE("bindings") {
  std::string a = "a";
  std::string b = "b";
  CHECK(BindingId(a) == BindingId("a"));
  CHECK(BindingId(b) == BindingId("b"));
  CHECK(BindingId("a") != BindingId("b"));
  CHECK(BindingId("a") != BindingId(b));
}

}  // namespace
}  // namespace match
