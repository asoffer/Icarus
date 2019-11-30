#include "ir/addr.h"

#include "test/catch.h"

namespace ir {
namespace {

TEST_CASE("heap") {
  int64_t n[10];
  auto a1 = Addr::Heap(&n[0]);
  auto a2 = Addr::Heap(&n[9]);

  CHECK(a1 + core::Bytes::Get<int64_t>() * 9 == a2);
  CHECK(core::Bytes::Get<int64_t>() * 9 + a1 == a2);
}

TEST_CASE("stack") {
  auto a1 = Addr::Stack(16);
  auto a2 = Addr::Stack(32);

  CHECK(a1 != a2);
  CHECK(a1 + core::Bytes(16) == a2);
  CHECK(core::Bytes(16) + a1 == a2);
  a1 += core::Bytes(16);
  CHECK(a1 == a2);
}

TEST_CASE("read-only") {
  auto a1 = Addr::ReadOnly(16);
  auto a2 = Addr::ReadOnly(32);

  CHECK(a1 != a2);
  CHECK(a1 + core::Bytes(16) == a2);
  CHECK(core::Bytes(16) + a1 == a2);
  a1 += core::Bytes(16);
  CHECK(a1 == a2);

  SECTION("stack and read-only are distinct") {
    auto s = Addr::Stack(32);
    CHECK(s != a2);
  }
}

}  // namespace
}  // namespace ir
