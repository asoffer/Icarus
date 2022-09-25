#include "core/type_system/function.h"

#include "core/type_system/sized_integer.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

TEST(TypeSystem, FunctionType) {
  TypeSystem<SizedIntegerType, ParameterType, FunctionType> type_system;
  Type t1 = SizedIntegerType::I(32);
  Type t2 = SizedIntegerType::U(32);
  Parameters<Type> params1;
  Parameters<Type> params2{
      Parameter<Type>{.name = "n", .value = t1},
  };
  Parameters<Type> params3{
      Parameter<Type>{.name = "n", .value = t1},
      Parameter<Type>{.name = "x", .value = t2},
  };

  ParameterType p1(type_system, params1);
  ParameterType p2(type_system, params2);
  ParameterType p3(type_system, params3);

  EXPECT_EQ(FunctionType(type_system, p1, {}),
            FunctionType(type_system, p1, {}));
  EXPECT_NE(FunctionType(type_system, p1, {}),
            FunctionType(type_system, p1, {t1}));
  EXPECT_NE(FunctionType(type_system, p1, {t1}),
            FunctionType(type_system, p1, {t2}));
  EXPECT_EQ(FunctionType(type_system, p2, {}),
            FunctionType(type_system, p2, {}));
  EXPECT_NE(FunctionType(type_system, p2, {}),
            FunctionType(type_system, p2, {t2}));
  EXPECT_NE(FunctionType(type_system, p2, {t1}),
            FunctionType(type_system, p2, {t2}));
  EXPECT_EQ(FunctionType(type_system, p3, {}),
            FunctionType(type_system, p3, {}));
  EXPECT_NE(FunctionType(type_system, p3, {}),
            FunctionType(type_system, p3, {t2}));
  EXPECT_NE(FunctionType(type_system, p3, {t1}),
            FunctionType(type_system, p3, {t2}));
  EXPECT_NE(FunctionType(type_system, p1, {}),
            FunctionType(type_system, p2, {}));
  EXPECT_NE(FunctionType(type_system, p1, {}),
            FunctionType(type_system, p2, {t2}));
  EXPECT_NE(FunctionType(type_system, p1, {t1}),
            FunctionType(type_system, p2, {t2}));
}

}  // namespace
}  // namespace core
