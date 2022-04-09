#include "compiler/builtin_module.h"

#include "compiler/instructions.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/instruction/set.h"

namespace compiler {
namespace {

using ::testing::Eq;
using ::testing::Field;
using ::testing::UnorderedElementsAre;

TEST(BuiltinModule, Abort) {
  auto module = MakeBuiltinModule();
  EXPECT_THAT(module->Symbols("abort"),
              UnorderedElementsAre(
                  Field(&module::Module::SymbolInformation::qualified_type,
                        Eq(type::QualType::Constant(type::Func({}, {}))))));
}

TEST(BuiltinModule, Alignment) {
  auto module = MakeBuiltinModule();
  ASSERT_THAT(
      module->Symbols("alignment"),
      UnorderedElementsAre(Field(
          &module::Module::SymbolInformation::qualified_type,
          Eq(type::QualType::Constant(type::Func(
              {core::AnonymousParameter(type::QualType::NonConstant(type::Type_))},
              {type::U64}))))));
  auto f = module->Symbols("alignment").begin()->value[0].get<ir::Fn>();

  EXPECT_EQ(
      EvaluateAtCompileTimeToBuffer(f.native(), type::Bool)[0].get<uint64_t>(),
      1);
  EXPECT_EQ(
      EvaluateAtCompileTimeToBuffer(f.native(), type::I64)[0].get<uint64_t>(),
      8);
}

TEST(BuiltinModule, Bytes) {
  auto module = MakeBuiltinModule();
  ASSERT_THAT(
      module->Symbols("bytes"),
      UnorderedElementsAre(Field(
          &module::Module::SymbolInformation::qualified_type,
          Eq(type::QualType::Constant(type::Func(
              {core::AnonymousParameter(type::QualType::NonConstant(type::Type_))},
              {type::U64}))))));
  auto f = module->Symbols("bytes").begin()->value[0].get<ir::Fn>();

  EXPECT_EQ(
      EvaluateAtCompileTimeToBuffer(f.native(), type::Bool)[0].get<uint64_t>(),
      1);
  EXPECT_EQ(
      EvaluateAtCompileTimeToBuffer(f.native(), type::I64)[0].get<uint64_t>(),
      8);
}

}  // namespace
}  // namespace compiler

