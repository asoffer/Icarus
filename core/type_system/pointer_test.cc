#include "core/type_system/pointer.h"

#include "core/type_system/finite_set.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

enum class T { T };
constexpr Type TT = FiniteSetType<T>(T::T);

TEST(TypeSystem, PointerType) {
  TypeSystem<FiniteSetType<T>, PointerType> type_system;
  Type t;
  PointerType p(type_system, t);
  PointerType p2(type_system, t);
  PointerType p3(type_system, p2);

  EXPECT_EQ(p, p2);
  EXPECT_NE(p, p3);
  EXPECT_NE(p2, p3);

  EXPECT_EQ(p.pointee(), t);
  EXPECT_EQ(p2.pointee(), t);
  EXPECT_EQ(p3.pointee(), p2);
}

}  // namespace
}  // namespace core
