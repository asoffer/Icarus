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

auto Vector(auto&& range) {
  std::vector<typename decltype(range.begin())::value_type> v;
  for (auto&& value : range) {
    v.push_back(std::move(value));
    std::cerr << v.back().qualified_type << "\n\n\n";
  }
  return v;
}

TEST(Builtin, Empty) {
  BuiltinModule m([](ir::Subroutine const&) -> ir::ByteCode { UNREACHABLE(); });
  EXPECT_THAT(Vector(m.Exported("x")), IsEmpty());
}

TEST(Builtin, HasElements) {
  BuiltinModule m([](ir::Subroutine const&) -> ir::ByteCode { UNREACHABLE(); });
  m.insert("x", {.qualified_type = type::QualType::Constant(type::Bool),
                 .visibility     = Module::Visibility::Exported});
  m.insert("z", {.qualified_type = type::QualType::Constant(type::I64),
                 .visibility     = Module::Visibility::Exported});
  m.insert("z", {.qualified_type = type::QualType::Constant(type::U64),
                 .visibility     = Module::Visibility::Exported});

  EXPECT_THAT(
      Vector(m.Exported("x")),
      UnorderedElementsAre(Field(&Module::SymbolInformation::qualified_type,
                                 Eq(type::QualType::Constant(type::Bool)))));
  EXPECT_THAT(Vector(m.Exported("y")), IsEmpty());
  EXPECT_THAT(
      Vector(m.Exported("z")),
      UnorderedElementsAre(Field(&Module::SymbolInformation::qualified_type,
                                 Eq(type::QualType::Constant(type::U64))),
                           Field(&Module::SymbolInformation::qualified_type,
                                 Eq(type::QualType::Constant(type::I64)))));
}

}  // namespace
}  // namespace module
