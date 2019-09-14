#include "ir/reg_or.h"

#include "test/catch.h"

namespace ir {
namespace {

TEST_CASE("implicit construction") {
  auto id = [](RegOr<int> r) { return r; };

  CHECK_FALSE(id(3).is_reg());
  CHECK(id(3).value() == 3);

  CHECK(id(Reg::Arg(3)).is_reg());
  CHECK(id(Reg::Arg(3)).reg() == Reg::Arg(3));
}

TEST_CASE("resolve") {
  auto f = [](Reg r) { return 4; };
  CHECK(RegOr<int>(Reg::Out(1)).resolve(f) == 4);
  CHECK(RegOr<int>(2).resolve(f) == 2);
}

TEST_CASE("apply") {
  auto f = [](auto x) {
    if constexpr (std::is_same_v<std::decay_t<decltype(x)>, Reg>) {
      return x.value();
    } else {
      return x * x;
    }
  };
  CHECK(RegOr<int>(Reg(1)).apply(f) == 1);
  CHECK(RegOr<int>(2).apply(f) == 4);
}

TEST_CASE("stringify") {
  CHECK(stringify(RegOr<int>(3)) == "3");
  CHECK(stringify(RegOr<bool>(true)) == "true");
  CHECK(stringify(RegOr<bool>(Reg(4))) == stringify(Reg(4)));
}

TEST_CASE("comparison") {
  CHECK(RegOr<int>(17) == RegOr<int>(17));
  CHECK_FALSE(RegOr<int>(17) != RegOr<int>(17));

  CHECK_FALSE(RegOr<int>(Reg(17)) == RegOr<int>(17));
  CHECK(RegOr<int>(Reg(17)) != RegOr<int>(17));
}

}  // namespace
}  // namespace ir
