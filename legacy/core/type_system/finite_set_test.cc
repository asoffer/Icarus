#include "core/type_system/finite_set.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

enum class E { A, B, C };
using Category   = FiniteSetType<E>;
using TS         = TypeSystem<Category>;
constexpr Type A = Category(nth::type<TS>, E::A);
constexpr Type B = Category(nth::type<TS>, E::B);
constexpr Type C = Category(nth::type<TS>, E::C);

TEST(TypeSystem, FiniteSetType) {
  EXPECT_EQ(A, A);
  EXPECT_EQ(B, B);
  EXPECT_EQ(C, C);
  EXPECT_NE(A, B);
  EXPECT_NE(A, C);
  EXPECT_NE(B, C);

  constexpr E a = Category(nth::type<TS>, E::A).value();
  constexpr E b = Category(nth::type<TS>, E::B).value();
  constexpr E c = Category(nth::type<TS>, E::C).value();
  EXPECT_EQ(a, E::A);
  EXPECT_EQ(b, E::B);
  EXPECT_EQ(c, E::C);
}

}  // namespace
}  // namespace core
