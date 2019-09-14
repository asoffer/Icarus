#include "ir/reg.h"

#include "test/catch.h"

namespace ir {
namespace {

TEST_CASE("construction") {
  SECTION("arg") {
    auto r = Reg::Arg(17);
    CHECK(r.is_arg());
    CHECK_FALSE(r.is_out());
    CHECK(r.arg_value() == 17u);
  }

  SECTION("out") {
    auto r = Reg::Out(17);
    CHECK(r.is_out());
    CHECK_FALSE(r.is_arg());
    CHECK(r.out_value() == 17u);
  }
}

TEST_CASE("stringify") {
  using base::stringify;
  CHECK(stringify(Reg(17)) == "r.17");
  CHECK(stringify(Reg::Arg(17)) == "arg.17");
  CHECK(stringify(Reg::Out(17)) == "out.17");
}

TEST_CASE("comparison") {
  CHECK(Reg(17) == Reg(17));
  CHECK_FALSE(Reg(17) != Reg(17));

  CHECK(Reg(1) != Reg(17));
  CHECK_FALSE(Reg(1) == Reg(17));

  CHECK(Reg(17) != Reg::Arg(17));
  CHECK(Reg(17) != Reg::Out(17));
  CHECK(Reg::Arg(17) != Reg::Out(17));

  CHECK(Reg::Arg(17) == Reg::Arg(17));
  CHECK(Reg::Out(17) == Reg::Out(17));
}

}  // namespace
}  // namespace ir
