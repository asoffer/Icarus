#include "core/type_system/type_system.h"

#include "core/type_system/builtin.h"
#include "core/type_system/pointer.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

TEST(TypeSystem, Categories) {
  static_assert(
      TypeSystemSupporting<TypeSystem<BuiltinType, PointerType>, BuiltinType>);
  static_assert(not TypeSystemSupporting<TypeSystem<PointerType>, BuiltinType>);
}

TEST(TypeSystem, Index) {
  TypeSystem<BuiltinType, PointerType> type_system;
  constexpr size_t builtin_index = type_system.index<BuiltinType>();
  constexpr size_t pointer_index = type_system.index<PointerType>();
  EXPECT_EQ(builtin_index, 0);
  EXPECT_EQ(pointer_index, 1);
}

TEST(TypeSystem, Access) {
  TypeSystem<BuiltinType, PointerType> type_system;

  BuiltinType i32 = BuiltinType::I<32>(type_system);
  PointerType p(type_system, i32);

  Type t = p;

  EXPECT_TRUE(t.is<PointerType>(type_system));
  EXPECT_FALSE(t.is<BuiltinType>(type_system));

  EXPECT_EQ(t.get<PointerType>(type_system), p);
  EXPECT_EQ(t.get_if<PointerType>(type_system), p);
  EXPECT_EQ(t.get_if<BuiltinType>(type_system), std::nullopt);
}

}  // namespace
}  // namespace core
