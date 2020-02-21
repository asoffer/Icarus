#include "base/unaligned_ref.h"

#include <cstring>

#include "gtest/gtest.h"

namespace base {
namespace {

TEST(UnalignedRef, HasReferenceSemantics) {
  {  // Mutable
    int n    = 3;
    auto ref = unaligned_ref<int>(n);
    n        = 4;
    int m    = ref;
    EXPECT_EQ(n, m);
  }

  {  // Const
    int n    = 3;
    auto ref = unaligned_ref<int const>(n);
    n        = 4;
    int m    = ref;
    EXPECT_EQ(n, m);
  }
}

TEST(UnalignedRef, ConvertsOnEquality) {
  {  // Mutable
    int n    = 3;
    auto ref = unaligned_ref<int>(n);
    EXPECT_EQ(n, ref);
    EXPECT_EQ(ref, n);
  }

  {  // Const
    int n    = 3;
    auto ref = unaligned_ref<int const>(n);
    EXPECT_EQ(n, ref);
    EXPECT_EQ(ref, n);
  }
}

TEST(UnalignedRef, ConvertsBothOnEquality) {
  {  // Mutable
    int a = 3, b = 3;
    auto a_ref = unaligned_ref<int>(a);
    auto b_ref = unaligned_ref<int>(b);

    EXPECT_EQ(a_ref, b_ref);

    a = 4;
    EXPECT_NE(a_ref, b_ref);
  }
  {  // Const
    int a = 3, b = 3;
    auto a_ref = unaligned_ref<int const>(a);
    auto b_ref = unaligned_ref<int const>(b);

    EXPECT_EQ(a_ref, b_ref);

    a = 4;
    EXPECT_NE(a_ref, b_ref);
  }
}

TEST(UnalignedRef, ConvertThroughMutability) {
  int n                            = 3;
  unaligned_ref<int> mut_ref       = n;
  unaligned_ref<int const> imm_ref = mut_ref;
  EXPECT_EQ(mut_ref, imm_ref);
}

TEST(UnalignedRef, AllowsUnaligned) {
  // Construct a buffer aligned appropriately but leave an extra byte at the end
  // so we can guarantee bad alignment for the purposes of this test.
  alignas(alignof(int)) char data[sizeof(int) + 1] = {};

  int num = 0x12345678;
  std::memcpy(&data[1], &num, sizeof(int));

  {  // Mutable
    auto ref = unaligned_ref<int>::FromPtr(&data[1]);
    EXPECT_EQ(ref, 0x12345678);
  }

  {  // Const
    auto ref = unaligned_ref<int const>::FromPtr(&data[1]);
    EXPECT_EQ(ref, 0x12345678);
  }
}

TEST(UnalignedRef, Assignment) {
  {  // Aligned
    int num  = 3;
    auto ref = unaligned_ref<int>(num);
    ref      = 4;
    EXPECT_EQ(num, 4);
  }

  {  // Unaligned
    alignas(alignof(int)) char data[sizeof(int) + 1] = {};

    int num = 0x12345678;
    std::memcpy(&data[1], &num, sizeof(int));

    auto ref = unaligned_ref<int>::FromPtr(&data[1]);
    ref      = 4;
    EXPECT_EQ(ref, 4);
  }
}

}  // namespace
}  // namespace base
