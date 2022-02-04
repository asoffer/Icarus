#include "type/primitive.h"

#include "base/universal_print.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "type/primitive.h"

namespace {

TEST(Primitive, Equality) {
  EXPECT_EQ(type::U8, type::U8);
  EXPECT_NE(type::U8, type::I8);
}

TEST(Primitive, ToString) {
  EXPECT_EQ(base::UniversalPrintToString(type::U8), "u8");
  EXPECT_EQ(base::UniversalPrintToString(type::Type_), "type");
}

TEST(Primitive, PointerDifferenceType) {
  EXPECT_EQ(type::PointerDifferenceType(core::Arch::Get<char[1], void (*)()>()),
            type::I8);
  EXPECT_EQ(type::PointerDifferenceType(core::Arch::Get<char[2], void (*)()>()),
            type::I16);
  EXPECT_EQ(type::PointerDifferenceType(core::Arch::Get<char[3], void (*)()>()),
            type::I32);
  EXPECT_EQ(type::PointerDifferenceType(core::Arch::Get<char[4], void (*)()>()),
            type::I32);
  EXPECT_EQ(type::PointerDifferenceType(core::Arch::Get<char[5], void (*)()>()),
            type::I64);
  EXPECT_EQ(type::PointerDifferenceType(core::Arch::Get<char[6], void (*)()>()),
            type::I64);
  EXPECT_EQ(type::PointerDifferenceType(core::Arch::Get<char[7], void (*)()>()),
            type::I64);
  EXPECT_EQ(type::PointerDifferenceType(core::Arch::Get<char[8], void (*)()>()),
            type::I64);
}

TEST(Primitive, MakePrimitive) {
  EXPECT_EQ(MakePrimitive(type::Primitive::Kind::U16), type::U16);
  EXPECT_EQ(MakePrimitive(type::Primitive::Kind::I16), type::I16);
  EXPECT_EQ(MakePrimitive(type::Primitive::Kind::Bool), type::Bool);
  EXPECT_EQ(MakePrimitive(type::Primitive::Kind::Module), type::Module);
  EXPECT_EQ(MakePrimitive(type::Primitive::Kind::Void), type::Void);
}
}  // namespace
