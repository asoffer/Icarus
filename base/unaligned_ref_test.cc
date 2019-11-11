#include <cstring>

#include "base/unaligned_ref.h"
#include "test/catch.h"

namespace base {
namespace {

TEST_CASE("has reference semantics") {
  int n    = 3;
  auto ref = unaligned_ref<int>(n);
  n        = 4;
  int m    = ref;
  CHECK(n == m);
}

TEST_CASE("Converts on equality") {
  int n    = 3;
  auto ref = unaligned_ref<int>(n);
  CHECK(n == ref);
  CHECK(ref == n);
}


TEST_CASE("Converts both on equality") {
  int a = 3, b = 3;
  auto a_ref = unaligned_ref<int>(a);
  auto b_ref = unaligned_ref<int>(b);

  CHECK(a_ref == b_ref);

  a = 4;
  CHECK(a_ref != b_ref);
}

TEST_CASE("Allows unaligned") {
  // Construct a buffer aligned appropriately but leave an extra byte at the end
  // so we can guarantee bad alignment for the purposes of this test.
  alignas(alignof(int)) char data[sizeof(int) + 1] = {}; 


  int num = 0x12345678;
  std::memcpy(&data[1], &num, sizeof(int));

  auto ref = unaligned_ref<int>(&data[1]);
  CHECK(ref == 0x12345678);
}

}  // namespace
}  // namespace base
