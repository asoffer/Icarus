#include "core/type_system/type_system.h"

#include "core/type_system/sized_integer.h"
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
      TypeSystemSupporting<TypeSystem<SizedIntegerType, PointerType>, SizedIntegerType>);
  static_assert(not TypeSystemSupporting<TypeSystem<PointerType>, SizedIntegerType>);
}

using TSys = TypeSystem<SizedIntegerType, PointerType>;

TEST(TypeSystem, Index) {
  TSys type_system;
  constexpr size_t sized_integer_index = type_system.index<SizedIntegerType>();
  constexpr size_t pointer_index       = type_system.index<PointerType>();
  EXPECT_EQ(sized_integer_index, 0);
  EXPECT_EQ(pointer_index, 1);
}

TEST(TypeSystem, Access) {
  TSys type_system;

  SizedIntegerType i32 = SizedIntegerType::I<32>();
  PointerType p(type_system, i32);

  Type t = p;

  EXPECT_TRUE(t.is<PointerType>(type_system));
  EXPECT_FALSE(t.is<SizedIntegerType>(type_system));

  EXPECT_EQ(t.get<PointerType>(type_system), p);
  EXPECT_EQ(t.get_if<PointerType>(type_system), p);
  EXPECT_EQ(t.get_if<SizedIntegerType>(type_system), std::nullopt);
}

TEST(TypeSystem, JasminConstruction) {
  using InstructionSet =
      jasmin::MakeInstructionSet<jasmin::Push, TSys::JasminInstructionSet>;
  using IrFunction = jasmin::Function<InstructionSet>;

  TSys type_system;
  Type i32 = SizedIntegerType::I<32>();

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

TEST(QualifiedType, QualifiedType) {
  using QT = QualifiedType<uint8_t>;

  TSys type_system;
  Type i32 = SizedIntegerType::I<32>();
  Type u32 = SizedIntegerType::U<32>();
  QT qt(i32, 5);
  EXPECT_TRUE(qt == QT(i32, 5));
  EXPECT_FALSE(qt != QT(i32, 5));
  EXPECT_EQ(qt.qualifiers(), 5);
  EXPECT_EQ(qt.type(), i32);

  EXPECT_FALSE(qt == QT(i32, 6));
  EXPECT_TRUE(qt != QT(i32, 6));
  EXPECT_FALSE(qt == QT(u32, 5));
  EXPECT_TRUE(qt != QT(u32, 5));
}

}  // namespace
}  // namespace core
