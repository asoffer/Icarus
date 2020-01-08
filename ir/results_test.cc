#include "ir/results.h"

#include "ir/reg.h"
#include "test/catch.h"

namespace ir {
namespace {

TEST_CASE("construction") {
  CHECK(Results().size() == 0u);
  CHECK(Results(3).size() == 1u);
  CHECK(Results(3, true, 4).size() == 3u);
}

TEST_CASE("Args pass through unchanged") {
  CHECK(Results(Reg::Arg(3)).get<Reg>(0) == Reg::Arg(3));
}

TEST_CASE("access values") {
  Results r(3, true, 4);
  CHECK(r.get<int>(0) == RegOr<int>(3));
  CHECK(r.get<bool>(1) == RegOr<bool>(true));
  CHECK(r.get<int>(2) == RegOr<int>(4));
}

TEST_CASE("access registers") {
  Results r;
  r.append(3);
  r.append(Reg(17));
  r.append(true);
  CHECK(r.get<int>(1) == RegOr<int>(Reg(17)));
  CHECK(r.get<Reg>(1) == Reg(17));
}

TEST_CASE("is reg") {
  Results r(3.14, Reg(4), Reg(17), true);
  CHECK_FALSE(r.is_reg(0));
  CHECK(r.is_reg(1));
  CHECK(r.is_reg(2));
  CHECK_FALSE(r.is_reg(0));
}

TEST_CASE("get result") {
  Results r(3.14, Reg(4), Reg(17), true);
  REQUIRE(r.GetResult(0).size() == 1u);
  CHECK(r.GetResult(0).get<double>(0) == RegOr<double>(3.14));
  REQUIRE(r.GetResult(1).size() == 1u);
  CHECK(r.GetResult(1).get<Reg>(0) == Reg(4));
  REQUIRE(r.GetResult(2).size() == 1u);
  CHECK(r.GetResult(2).get<Reg>(0) == Reg(17));
  REQUIRE(r.GetResult(3).size() == 1u);
  CHECK(r.GetResult(3).get<bool>(0) == RegOr<bool>(true));
}

TEST_CASE("get result starting with reg") {
  Results r(Reg(1), static_cast<int64_t>(4));
  REQUIRE(r.GetResult(0).size() == 1u);
  CHECK(r.GetResult(0).get<Reg>(0) == Reg(1));
  REQUIRE(r.GetResult(1).size() == 1u);
  CHECK(r.GetResult(1).get<int64_t>(0) ==
        RegOr<int64_t>(static_cast<int64_t>(4)));
}

TEST_CASE("append results") {
  Results r1(true, Reg(17));
  Results r2(3, Reg(34));
  r1.append(r2);

  REQUIRE(r1.size() == 4u);
  CHECK(r1.get<bool>(0) == RegOr<bool>(true));
  CHECK(r1.get<Reg>(1) == Reg(17));
  CHECK(r1.get<int>(2) == RegOr<int>(3));
  CHECK(r1.get<Reg>(3) == Reg(34));
}

TEST_CASE("FromRaw") {
  int32_t n    = 0xdeadbeef;
  auto results = Results::FromRaw(static_cast<void *>(&n), core::Bytes(4));
  CHECK(results.size() == 1);
  CHECK(results.get<int32_t>(0) == RegOr<int32_t>(n));
}

TEST_CASE("FromUntypedBuffer") {
  base::untyped_buffer buf;
  buf.append(1234);
  buf.append(true);
  buf.append(5678);
  auto results = Results::FromUntypedBuffer({0, sizeof(int), sizeof(int) + 1},
                                            std::move(buf));
  CHECK(results.size() == 3);
  CHECK(results.get<int>(0) == RegOr<int>(1234));
  CHECK(results.get<bool>(1) == RegOr<bool>(true));
  CHECK(results.get<int>(2) == RegOr<int>(5678));
}

TEST_CASE("to_string") {
  CHECK(Results().to_string() == "[]");
  CHECK(Results(true).to_string() == "[offset(0)]");
  CHECK(Results(uint16_t{3}, true, uint64_t{4}, Reg::Arg(4)).to_string() ==
        "[offset(0), offset(2), offset(3), arg.4]");
}

TEST_CASE("for_each_reg") {
  auto f = [](Reg& r) { r = Reg(r.value() + 100); };

  SECTION("empty") {
    Results r(1, 2, 3);
    r.for_each_reg(f);
    CHECK(r.get<int>(0) == RegOr<int>(1));
    CHECK(r.get<int>(1) == RegOr<int>(2));
    CHECK(r.get<int>(2) == RegOr<int>(3));
  }

  SECTION("Updates") {
    Results r(1, Reg(2), Reg(3));
    r.for_each_reg(f);
    CHECK(r.get<int>(0) == RegOr<int>(1));
    CHECK(r.get<Reg>(1) == Reg(102));
    CHECK(r.get<Reg>(2) == Reg(103));
  }
}
}  // namespace
}  // namespace ir
