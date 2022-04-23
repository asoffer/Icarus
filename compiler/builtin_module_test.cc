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
  module::SharedContext ctx(MakeBuiltinModule());
  auto module = ctx.module_table().module(ir::ModuleId::Builtin());
  ASSERT_THAT(module->Symbols("alignment"),
              UnorderedElementsAre(
                  Field(&module::Module::SymbolInformation::qualified_type,
                        Eq(type::QualType::Constant(type::Func(
                            {core::AnonymousParameter(
                                type::QualType::NonConstant(type::Type_))},
                            {type::U64}))))));
  auto f = module->Symbols("alignment").begin()->value[0].get<ir::Fn>();

  EXPECT_EQ(
      EvaluateAtCompileTimeToBuffer(ctx, f, type::Bool)[0].get<uint64_t>(), 1);
  EXPECT_EQ(EvaluateAtCompileTimeToBuffer(ctx, f, type::I64)[0].get<uint64_t>(),
            8);
}

TEST(BuiltinModule, Bytes) {
  module::SharedContext ctx(MakeBuiltinModule());
  auto module = ctx.module_table().module(ir::ModuleId::Builtin());
  ASSERT_THAT(module->Symbols("bytes"),
              UnorderedElementsAre(
                  Field(&module::Module::SymbolInformation::qualified_type,
                        Eq(type::QualType::Constant(type::Func(
                            {core::AnonymousParameter(
                                type::QualType::NonConstant(type::Type_))},
                            {type::U64}))))));
  auto f = module->Symbols("bytes").begin()->value[0].get<ir::Fn>();

  EXPECT_EQ(
      EvaluateAtCompileTimeToBuffer(ctx, f, type::Bool)[0].get<uint64_t>(), 1);
  EXPECT_EQ(EvaluateAtCompileTimeToBuffer(ctx, f, type::I64)[0].get<uint64_t>(),
            8);
}

TEST(BuiltinModule, AsciiEncode) {
  module::SharedContext ctx(MakeBuiltinModule());
  auto module = ctx.module_table().module(ir::ModuleId::Builtin());
  ASSERT_THAT(module->Symbols("ascii_encode"),
              UnorderedElementsAre(
                  Field(&module::Module::SymbolInformation::qualified_type,
                        Eq(type::QualType::Constant(type::Func(
                            {core::AnonymousParameter(
                                type::QualType::NonConstant(type::U8))},
                            {type::Char}))))));
  auto f = module->Symbols("ascii_encode").begin()->value[0].get<ir::Fn>();

  EXPECT_EQ(
      EvaluateAtCompileTimeToBuffer(ctx, f, uint8_t{7})[0].get<ir::Char>(),
      ir::Char(7));
  EXPECT_EQ(
      EvaluateAtCompileTimeToBuffer(ctx, f, uint8_t{64})[0].get<ir::Char>(),
      ir::Char(65));
}

TEST(BuiltinModule, AsciiDecode) {
  module::SharedContext ctx(MakeBuiltinModule());
  auto module = ctx.module_table().module(ir::ModuleId::Builtin());
  ASSERT_THAT(module->Symbols("ascii_decode"),
              UnorderedElementsAre(
                  Field(&module::Module::SymbolInformation::qualified_type,
                        Eq(type::QualType::Constant(type::Func(
                            {core::AnonymousParameter(
                                type::QualType::NonConstant(type::U8))},
                            {type::Char}))))));
  auto f = module->Symbols("ascii_decode").begin()->value[0].get<ir::Fn>();

  EXPECT_EQ(
      EvaluateAtCompileTimeToBuffer(ctx, f, ir::Char(7))[0].get<ir::Char>(),
      uint8_t{7});
  EXPECT_EQ(
      EvaluateAtCompileTimeToBuffer(ctx, f, ir::Char(64))[0].get<ir::Char>(),
      uint8_t{65});
}


}  // namespace
}  // namespace compiler
