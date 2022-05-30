#include "module/module.h"

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "module/mock_module.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace module {
namespace {

using ::testing::_;
using ::testing::Eq;
using ::testing::Field;
using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

auto Vector(auto&& range) {
  std::vector<typename decltype(range.begin())::value_type> v;
  for (auto&& value : range) {
    v.push_back(std::move(value));
    std::cerr << v.back().qualified_type << "\n\n\n";
  }
  return v;
}

TEST(Module, ExportedAndPrivate) {
  MockModule m("", ir::ModuleId(17));

  absl::flat_hash_map<std::string, std::vector<Module::SymbolInformation>> const
      symbols = {
          {"empty", {}},
          {"one_exported",
           {{.qualified_type = type::QualType::Constant(type::I64),
             .visibility     = Module::Visibility::Exported}}},
          {"one_private",
           {{.qualified_type = type::QualType::Constant(type::I64),
             .visibility     = Module::Visibility::Private}}},
          {"private_then_exported",
           {{.qualified_type = type::QualType::Constant(type::I32),
             .visibility     = Module::Visibility::Private},
            {.qualified_type = type::QualType::Constant(type::F32),
             .visibility     = Module::Visibility::Exported}}},
          {"exported_then_private",
           {{.qualified_type = type::QualType::Constant(type::I64),
             .visibility     = Module::Visibility::Exported},
            {.qualified_type = type::QualType::Constant(type::F64),
             .visibility     = Module::Visibility::Private}}},
      };
  ON_CALL(m, Symbols)
      .WillByDefault([&](std::string_view s)
                         -> absl::Span<Module::SymbolInformation const> {
        return symbols.at(s);
      });

  ON_CALL(m, SymbolsByName)
      .WillByDefault([&](absl::FunctionRef<void(
                             std::string_view,
                             absl::Span<Module::SymbolInformation const>)>
                             f) {
        for (auto const& [name, symbol_range] : symbols) {
          f(name, symbol_range);
        }
      });

  EXPECT_THAT(Vector(m.Exported("empty")), IsEmpty());
  EXPECT_THAT(Vector(m.Private("empty")), IsEmpty());

  EXPECT_THAT(Vector(m.Exported("one_private")), IsEmpty());
  EXPECT_THAT(
      Vector(m.Private("one_private")),
      UnorderedElementsAre(Field(&Module::SymbolInformation::qualified_type,
                                 Eq(type::QualType::Constant(type::I64)))));
  EXPECT_THAT(Vector(m.Private("one_exported")), IsEmpty());
  EXPECT_THAT(
      Vector(m.Exported("one_exported")),
      UnorderedElementsAre(Field(&Module::SymbolInformation::qualified_type,
                                 Eq(type::QualType::Constant(type::I64)))));

  EXPECT_THAT(
      Vector(m.Private("private_then_exported")),
      UnorderedElementsAre(Field(&Module::SymbolInformation::qualified_type,
                                 Eq(type::QualType::Constant(type::I32)))));
  EXPECT_THAT(
      Vector(m.Exported("private_then_exported")),
      UnorderedElementsAre(Field(&Module::SymbolInformation::qualified_type,
                                 Eq(type::QualType::Constant(type::F32)))));

  EXPECT_THAT(
      Vector(m.Private("exported_then_private")),
      UnorderedElementsAre(Field(&Module::SymbolInformation::qualified_type,
                                 Eq(type::QualType::Constant(type::F64)))));
  EXPECT_THAT(
      Vector(m.Exported("exported_then_private")),
      UnorderedElementsAre(Field(&Module::SymbolInformation::qualified_type,
                                 Eq(type::QualType::Constant(type::I64)))));

  absl::flat_hash_map<std::string_view, std::vector<Module::SymbolInformation>>
      exported_symbols;
  m.ExportedSymbolsByName(
      [&](std::string_view name, auto const& exported_range) {
        auto& v = exported_symbols.try_emplace(name).first->second;
        for (auto const& exported : exported_range) { v.push_back(exported); }
      });
  EXPECT_THAT(
      exported_symbols,
      UnorderedElementsAre(
          Pair("one_exported", UnorderedElementsAre(Field(
                                   &Module::SymbolInformation::qualified_type,
                                   Eq(type::QualType::Constant(type::I64))))),
          Pair("private_then_exported",
               UnorderedElementsAre(
                   Field(&Module::SymbolInformation::qualified_type,
                         Eq(type::QualType::Constant(type::F32))))),
          Pair("exported_then_private",
               UnorderedElementsAre(
                   Field(&Module::SymbolInformation::qualified_type,
                         Eq(type::QualType::Constant(type::I64)))))));
}

}  // namespace
}  // namespace module
