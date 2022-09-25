#include "core/type_system/finite_set.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

enum class E { A, B, C };
using Category   = FiniteSetType<E>;
using TS         = TypeSystem<Category>;
constexpr Type A = Category(base::meta<TS>, E::A);
constexpr Type B = Category(base::meta<TS>, E::B);
constexpr Type C = Category(base::meta<TS>, E::C);

TEST(TypeSystem, FiniteSetType) {
  EXPECT_EQ(A, A);
  EXPECT_EQ(B, B);
  EXPECT_EQ(C, C);
  EXPECT_NE(A, B);
  EXPECT_NE(A, C);
  EXPECT_NE(B, C);

  constexpr E a = Category(base::meta<TS>, E::A).value();
  constexpr E b = Category(base::meta<TS>, E::B).value();
  constexpr E c = Category(base::meta<TS>, E::C).value();
  EXPECT_EQ(a, E::A);
  EXPECT_EQ(b, E::B);
  EXPECT_EQ(c, E::C);
}

}  // namespace
}  // namespace core
