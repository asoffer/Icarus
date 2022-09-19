#include "core/type_system/parameter.h"

#include "core/type_system/builtin.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

TEST(TypeSystem, Parameter) {
  TypeSystem<BuiltinType, ParameterType> type_system;
  Type t1 = BuiltinType::I<32>(type_system);
  Type t2 = BuiltinType::U<32>(type_system);
  Parameters<Type> p1;
  Parameters<Type> p2{
      Parameter<Type>{.name = "n", .value = t1},
  };
  Parameters<Type> p3{
      Parameter<Type>{.name = "n", .value = t1},
      Parameter<Type>{.name = "x", .value = t2},
  };

  EXPECT_EQ(ParameterType(type_system, p1), ParameterType(type_system, p1));
  EXPECT_EQ(ParameterType(type_system, p2), ParameterType(type_system, p2));
  EXPECT_EQ(ParameterType(type_system, p3), ParameterType(type_system, p3));

  EXPECT_NE(ParameterType(type_system, p1), ParameterType(type_system, p2));
  EXPECT_NE(ParameterType(type_system, p1), ParameterType(type_system, p3));
  EXPECT_NE(ParameterType(type_system, p2), ParameterType(type_system, p3));
}

}  // namespace
}  // namespace core


