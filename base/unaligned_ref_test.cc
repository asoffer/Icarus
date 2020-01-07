#include <cstring>

#include "base/unaligned_ref.h"
#include "test/catch.h"

namespace base {
namespace {

TEST_CASE("has reference semantics") {
  SECTION("Mutable") {
    int n    = 3;
    auto ref = unaligned_ref<int>(n);
    n        = 4;
    int m    = ref;
    CHECK(n == m);
  }

  SECTION("Const") {
    int n    = 3;
    auto ref = unaligned_ref<int const>(n);
    n        = 4;
    int m    = ref;
    CHECK(n == m);
  }
}

TEST_CASE("Converts on equality") {
  SECTION("Mutable") {
    int n    = 3;
    auto ref = unaligned_ref<int>(n);
    CHECK(n == ref);
    CHECK(ref == n);
  }

  SECTION("Const") {
    int n    = 3;
    auto ref = unaligned_ref<int const>(n);
    CHECK(n == ref);
    CHECK(ref == n);
  }
}

TEST_CASE("Converts both on equality") {
  SECTION("Mutable") {
    int a = 3, b = 3;
    auto a_ref = unaligned_ref<int>(a);
    auto b_ref = unaligned_ref<int>(b);

    CHECK(a_ref == b_ref);

    a = 4;
    CHECK(a_ref != b_ref);
  }
  SECTION("Const") {
    int a = 3, b = 3;
    auto a_ref = unaligned_ref<int const>(a);
    auto b_ref = unaligned_ref<int const>(b);

    CHECK(a_ref == b_ref);

    a = 4;
    CHECK(a_ref != b_ref);
  }
}

TEST_CASE("Convert through mutability") {
  int n                            = 3;
  unaligned_ref<int> mut_ref       = n;
  unaligned_ref<int const> imm_ref = mut_ref;
  CHECK(mut_ref == imm_ref);
}

TEST_CASE("Allows unaligned") {
  // Construct a buffer aligned appropriately but leave an extra byte at the end
  // so we can guarantee bad alignment for the purposes of this test.
  alignas(alignof(int)) char data[sizeof(int) + 1] = {};

  int num = 0x12345678;
  std::memcpy(&data[1], &num, sizeof(int));

  SECTION("Mutable") {
    auto ref = unaligned_ref<int>::FromPtr(&data[1]);
    CHECK(ref == 0x12345678);
  }

  SECTION("Const") {
    auto ref = unaligned_ref<int const>::FromPtr(&data[1]);
    CHECK(ref == 0x12345678);
  }
}

TEST_CASE("Assignment") {
  SECTION("Aligned") {
    int num  = 3;
    auto ref = unaligned_ref<int>(num);
    ref      = 4;
    CHECK(num == 4);
  }

  SECTION("Unaligned") {
    alignas(alignof(int)) char data[sizeof(int) + 1] = {};

    int num = 0x12345678;
    std::memcpy(&data[1], &num, sizeof(int));

    auto ref = unaligned_ref<int>::FromPtr(&data[1]);
    ref      = 4;
    CHECK(ref == 4);
  }
}

}  // namespace
}  // namespace base
