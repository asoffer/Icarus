#include "ir/results.h"

#include "test/catch.h"

namespace ir {
namespace {

TEST_CASE("construction") {
  CHECK(Results{}.size() == 0u);
  CHECK(Results{3}.size() == 1u);
  CHECK(Results(3, true, 4).size() == 3u);
}

TEST_CASE("access values") {
  Results r{3, true, 4};
  CHECK(r.get<int>(0) == RegisterOr{3});
  CHECK(r.get<bool>(1) == RegisterOr{true});
  CHECK(r.get<int>(2) == RegisterOr{4});
}

TEST_CASE("access registers") {
  Results r;
  r.append(3);
  r.append(Reg{17});
  r.append(true);
  CHECK(r.get<int>(1) == RegisterOr<int>{Reg{17}});
  CHECK(r.get<Reg>(1) == Reg{17});
}

TEST_CASE("is reg") {
  Results r{3.14, Reg{4}, Reg{17}, true};
  CHECK_FALSE(r.is_reg(0));
  CHECK(r.is_reg(1));
  CHECK(r.is_reg(2));
  CHECK_FALSE(r.is_reg(0));
}

TEST_CASE("get result") {
  Results r{3.14, Reg{4}, Reg{17}, true};
  REQUIRE(r.GetResult(0).size() == 1u);
  CHECK(r.GetResult(0).get<double>(0) == RegisterOr{3.14});
  REQUIRE(r.GetResult(1).size() == 1u);
  CHECK(r.GetResult(1).get<Reg>(0) == Reg{4});
  REQUIRE(r.GetResult(2).size() == 1u);
  CHECK(r.GetResult(2).get<Reg>(0) == Reg{17});
  REQUIRE(r.GetResult(3).size() == 1u);
  CHECK(r.GetResult(3).get<bool>(0) == RegisterOr{true});
}

TEST_CASE("append results") {
  Results r1{true, Reg{17}};
  Results r2{3, Reg{34}};
  r1.append(r2);

  REQUIRE(r1.size() == 4u);
  CHECK(r1.get<bool>(0) == RegisterOr{true});
  CHECK(r1.get<Reg>(1) == Reg{17});
  CHECK(r1.get<int>(2) == RegisterOr{3});
  CHECK(r1.get<Reg>(3) == Reg{34});
}

TEST_CASE("FromRaw") {
  int n = 0xdeadbeef;
  auto results = Results::FromRaw(static_cast<void *>(&n), layout::Bytes{4});
  CHECK(results.size() == 1);
  CHECK(results.get<int>(0) == RegisterOr{n});
}

}  // namespace
}  // namespace ir
