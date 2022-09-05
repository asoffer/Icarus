#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/value/slice.h"
#include "semantic_analysis/type_verification/matchers.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/primitive.h"
#include "type/slice.h"

namespace semantic_analysis {
namespace {

using ::testing::ElementsAre;

TEST(Terminal, BooleanPrimitives) {
  EXPECT_THAT(ast::Terminal("true", true),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Bool))));
  EXPECT_THAT(ast::Terminal("false", false),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Bool))));
}

ir::Slice MakeSlice(char const *cstr) {
  return ir::Slice(reinterpret_cast<std::byte *>(const_cast<char *>(cstr)),
                   std::strlen(cstr));
}

TEST(Terminal, StringLiterals) {
  EXPECT_THAT(ast::Terminal(R"("")", MakeSlice("")),
              HasQualTypes(ElementsAre(
                  type::QualType::Constant(type::Slc(type::Char)))));
  EXPECT_THAT(ast::Terminal(R"("abc")", MakeSlice("abc")),
              HasQualTypes(ElementsAre(
                  type::QualType::Constant(type::Slc(type::Char)))));
  EXPECT_THAT(ast::Terminal(R"("ab\"c")", MakeSlice("abc\"")),
              HasQualTypes(ElementsAre(
                  type::QualType::Constant(type::Slc(type::Char)))));
  EXPECT_THAT(ast::Terminal(R"("ab\n\r\t\v\\c")", MakeSlice("ab\n\r\t\v\\c")),
              HasQualTypes(ElementsAre(
                  type::QualType::Constant(type::Slc(type::Char)))));
}

TEST(Terminal, Types) {
  EXPECT_THAT(ast::Terminal("byte", type::Byte),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("bool", type::Bool),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("u8", type::U8),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("u16", type::U16),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("u32", type::U32),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("u64", type::U64),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("i8", type::I8),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("i16", type::I16),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("i32", type::I32),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("i64", type::I64),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("f32", type::F32),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("f64", type::F64),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("integer", type::Integer),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("type", type::Type_),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
  EXPECT_THAT(ast::Terminal("module", type::Module),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Type_))));
}

TEST(Terminal, Numbers) {
  EXPECT_THAT(
      ast::Terminal("1234", ir::Integer(1234)),
      HasQualTypes(ElementsAre(type::QualType::Constant(type::Integer))));
  EXPECT_THAT(ast::Terminal("12.34", 12.34),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::F64))));
}

TEST(Terminal, Characters) {
  EXPECT_THAT(ast::Terminal("!'a'", ir::Char('a')),
              HasQualTypes(ElementsAre(type::QualType::Constant(type::Char))));
}

TEST(Terminal, Null) {
  EXPECT_THAT(
      ast::Terminal("null", ir::Null()),
      HasQualTypes(ElementsAre(type::QualType::Constant(type::NullPtr))));
}

}  // namespace
}  // namespace semantic_analysis
