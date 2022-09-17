#include "core/type_system/type_system.h"

#include "core/type_system/builtin.h"
#include "core/type_system/pointer.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "jasmin/execute.h"
#include "jasmin/function.h"
#include "jasmin/instructions/core.h"

namespace core {
namespace {

TEST(TypeSystem, Categories) {
  static_assert(
      TypeSystemSupporting<TypeSystem<BuiltinType, PointerType>, BuiltinType>);
  static_assert(not TypeSystemSupporting<TypeSystem<PointerType>, BuiltinType>);
}

using TSys = TypeSystem<BuiltinType, PointerType>;

TEST(TypeSystem, Index) {
  TSys type_system;
  constexpr size_t builtin_index = type_system.index<BuiltinType>();
  constexpr size_t pointer_index = type_system.index<PointerType>();
  EXPECT_EQ(builtin_index, 0);
  EXPECT_EQ(pointer_index, 1);
}

TEST(TypeSystem, Access) {
  TSys type_system;

  BuiltinType i32 = BuiltinType::I<32>(type_system);
  PointerType p(type_system, i32);

  Type t = p;

  EXPECT_TRUE(t.is<PointerType>(type_system));
  EXPECT_FALSE(t.is<BuiltinType>(type_system));

  EXPECT_EQ(t.get<PointerType>(type_system), p);
  EXPECT_EQ(t.get_if<PointerType>(type_system), p);
  EXPECT_EQ(t.get_if<BuiltinType>(type_system), std::nullopt);
}

TEST(TypeSystem, JasminConstruction) {
  using InstructionSet =
      jasmin::MakeInstructionSet<jasmin::Push, TSys::JasminInstructionSet>;
  using IrFunction = jasmin::Function<InstructionSet>;

  TSys type_system;
  Type i32 = BuiltinType::I<32>(type_system);

  Type result;
  {
    IrFunction f(0, 1);
    f.append<jasmin::Push>(i32);
    f.append<jasmin::Push>(&type_system);
    f.append<TSys::Make<PointerType>>();
    f.append<jasmin::Return>();
    jasmin::Execute(f, {}, result);
  }

  EXPECT_EQ(result, PointerType(type_system, i32));

  {
    IrFunction f(0, 1);
    f.append<jasmin::Push>(i32);
    f.append<jasmin::Push>(&type_system);
    f.append<TSys::Make<PointerType>>();
    f.append<jasmin::Push>(&type_system);
    f.append<TSys::Make<PointerType>>();
    f.append<jasmin::Return>();
    jasmin::Execute(f, {}, result);
  }

  EXPECT_EQ(result, PointerType(type_system, PointerType(type_system, i32)));
}

}  // namespace
}  // namespace core
