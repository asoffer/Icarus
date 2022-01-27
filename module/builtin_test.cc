#include "module/builtin.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace module {
namespace {
using ::testing::Eq;
using ::testing::Field;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

TEST(Builtin, Empty) {
  BuiltinModule m;
  EXPECT_THAT(m.Exported("x"), IsEmpty());
}

TEST(Builtin, HasElements) {
  BuiltinModule m;
  m.insert("x", {.qualified_type = type::QualType::Constant(type::Bool)});
  m.insert("z", {.qualified_type = type::QualType::Constant(type::I64)});
  m.insert("z", {.qualified_type = type::QualType::Constant(type::U64)});

  EXPECT_THAT(m.Exported("x"), UnorderedElementsAre(Field(
                                   &Module::SymbolInformation::qualified_type,
                                   Eq(type::QualType::Constant(type::Bool)))));
  EXPECT_THAT(m.Exported("y"), IsEmpty());
  EXPECT_THAT(
      m.Exported("z"),
      UnorderedElementsAre(Field(&Module::SymbolInformation::qualified_type,
                                 Eq(type::QualType::Constant(type::U64))),
                           Field(&Module::SymbolInformation::qualified_type,
                                 Eq(type::QualType::Constant(type::I64)))));
}

}  // namespace
}  // namespace compiler
