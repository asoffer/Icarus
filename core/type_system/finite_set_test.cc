#include "core/type_system/finite_set.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

enum class E { A, B, C };
using Category   = FiniteSetType<E>;
constexpr Type A = Category(E::A);
constexpr Type B = Category(E::B);
constexpr Type C = Category(E::C);

TEST(TypeSystem, FiniteSetType) {
  EXPECT_EQ(A, A);
  EXPECT_EQ(B, B);
  EXPECT_EQ(C, C);
  EXPECT_NE(A, B);
  EXPECT_NE(A, C);
  EXPECT_NE(B, C);

  constexpr E a = Category(E::A).value();
  constexpr E b = Category(E::B).value();
  constexpr E c = Category(E::C).value();
  EXPECT_EQ(a, E::A);
  EXPECT_EQ(b, E::B);
  EXPECT_EQ(c, E::C);
}

}  // namespace
}  // namespace core
