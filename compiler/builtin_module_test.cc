#include "compiler/builtin_module.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/interpreter/interpreter.h"
#include "ir/value/matcher.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using ::ir::ResultBufferHolding;
using ::ir::ValueOfType;
using ::testing::Eq;
using ::testing::Field;
using ::testing::Optional;
using ::testing::UnorderedElementsAre;

TEST(BuiltinModule, Abort) {
  auto module = MakeBuiltinModule();
  EXPECT_THAT(module->Symbols("abort"),
              UnorderedElementsAre(
                  Field(&module::Module::SymbolInformation::qualified_type,
                        Eq(type::QualType::Constant(type::Func({}, {}))))));
}

TEST(BuiltinModule, Alignment) {
  auto fn_qt = type::QualType::Constant(type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::Type_))},
      {type::U64}));

  module::SharedContext ctx(MakeBuiltinModule());
  auto module = ctx.module_table().module(ir::ModuleId::Builtin());
  ASSERT_THAT(
      module->Symbols("alignment"),
      UnorderedElementsAre(Field(
          &module::Module::SymbolInformation::qualified_type, Eq(fn_qt))));
  auto f = module->Symbols("alignment").begin()->value[0].get<ir::Fn>();

  std::optional<ir::CompleteResultBuffer> result;

  result =
      ir::interpreter::Interpret(ctx, f, ir::CompleteResultBuffer(type::Bool));
  EXPECT_THAT(result, Optional(ResultBufferHolding(ValueOfType<uint64_t>(1))));

  result =
      ir::interpreter::Interpret(ctx, f, ir::CompleteResultBuffer(type::I64));
  EXPECT_THAT(result, Optional(ResultBufferHolding(ValueOfType<uint64_t>(8))));
}

TEST(BuiltinModule, Bytes) {
  auto fn_qt = type::QualType::Constant(type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::Type_))},
      {type::U64}));
  module::SharedContext ctx(MakeBuiltinModule());
  auto module = ctx.module_table().module(ir::ModuleId::Builtin());
  ASSERT_THAT(
      module->Symbols("bytes"),
      UnorderedElementsAre(Field(
          &module::Module::SymbolInformation::qualified_type, Eq(fn_qt))));
  auto f = module->Symbols("bytes").begin()->value[0].get<ir::Fn>();

  std::optional<ir::CompleteResultBuffer> result;

  result =
      ir::interpreter::Interpret(ctx, f, ir::CompleteResultBuffer(type::Bool));
  EXPECT_THAT(result, Optional(ResultBufferHolding(ValueOfType<uint64_t>(1))));

  result =
      ir::interpreter::Interpret(ctx, f, ir::CompleteResultBuffer(type::I64));
  EXPECT_THAT(result, Optional(ResultBufferHolding(ValueOfType<uint64_t>(8))));
}

TEST(BuiltinModule, AsciiEncode) {
  auto fn_qt = type::QualType::Constant(type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::U8))},
      {type::Char}));
  module::SharedContext ctx(MakeBuiltinModule());
  auto module = ctx.module_table().module(ir::ModuleId::Builtin());
  ASSERT_THAT(
      module->Symbols("ascii_encode"),
      UnorderedElementsAre(Field(
          &module::Module::SymbolInformation::qualified_type, Eq(fn_qt))));
  auto f = module->Symbols("ascii_encode").begin()->value[0].get<ir::Fn>();

  std::optional<ir::CompleteResultBuffer> result;

  result =
      ir::interpreter::Interpret(ctx, f, ir::CompleteResultBuffer(uint8_t{7}));
  EXPECT_THAT(
      result,
      Optional(ResultBufferHolding(ValueOfType<ir::Char>(ir::Char('\7')))));
  result =
      ir::interpreter::Interpret(ctx, f, ir::CompleteResultBuffer(uint8_t{65}));
  EXPECT_THAT(
      result,
      Optional(ResultBufferHolding(ValueOfType<ir::Char>(ir::Char('A')))));
}

TEST(BuiltinModule, AsciiDecode) {
  auto fn_qt = type::QualType::Constant(type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::Char))},
      {type::U8}));
  module::SharedContext ctx(MakeBuiltinModule());
  auto module = ctx.module_table().module(ir::ModuleId::Builtin());
  ASSERT_THAT(
      module->Symbols("ascii_decode"),
      UnorderedElementsAre(Field(
          &module::Module::SymbolInformation::qualified_type, Eq(fn_qt))));
  auto f = module->Symbols("ascii_decode").begin()->value[0].get<ir::Fn>();

  std::optional<ir::CompleteResultBuffer> result;

  result = ir::interpreter::Interpret(ctx, f,
                                      ir::CompleteResultBuffer(ir::Char('\7')));
  EXPECT_THAT(result, Optional(ResultBufferHolding(ValueOfType<uint8_t>(7))));
  result = ir::interpreter::Interpret(ctx, f,
                                      ir::CompleteResultBuffer(ir::Char('A')));
  EXPECT_THAT(result, Optional(ResultBufferHolding(ValueOfType<uint8_t>(65))));
}

}  // namespace
}  // namespace compiler
