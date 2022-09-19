#include "core/type_system/pointer.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

TEST(TypeSystem, PointerType) {
  TypeSystem<PointerType> type_system;
  Type t;
  PointerType p(type_system, t);
  PointerType p2(type_system, t);
  PointerType p3(type_system, p2);

  EXPECT_EQ(p, p2);
  EXPECT_NE(p, p3);
  EXPECT_NE(p2, p3);
}

}  // namespace
}  // namespace core
