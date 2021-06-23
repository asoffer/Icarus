#include "compiler/compiler.h"
#include "compiler/library_module.h"
#include "frontend/source/buffer.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::Eq;
using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(BuiltinReserveMemory, FunctionSuccess) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(reserve_memory(1 as u64, 1 as u64))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::NonConstant(type::MemPtr));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinReserveMemory, NonConstantArgument) {
  test::TestModule mod;
  mod.AppendCode(R"(n := 3 as u64)");
  auto const *call  = mod.Append<ast::Call>(R"(reserve_memory(n, 1 as u64))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::NonConstant(type::MemPtr));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinReserveMemory, WrongType) {
  test::TestModule mod;
  auto const *call =
      mod.Append<ast::Call>(R"(reserve_memory(true, 1 as u64))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::NonConstant(type::MemPtr));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, FunctionSuccess) {
  test::TestModule mod;
  auto const *call =
      mod.Append<ast::Call>(R"(foreign("my_function", i64 -> bool))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt,
            type::QualType::Constant(type::Func(
                {core::AnonymousParam(type::QualType::NonConstant(type::I64))},
                {type::Bool})));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinForeign, PointerSuccess) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(foreign("my_ptr", *i64))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Ptr(type::I64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinForeign, BufferPointerSuccess) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(foreign("my_array", [*]i64))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::BufPtr(type::I64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinForeign, NamedArgs) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(
      foreign(name = "my_function", foreign_type = i64 -> bool)
      )");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, NoArgs) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("foreign()");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, OneArg) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(foreign("abc"))");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, TooManyArgs) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(foreign("abc", 1, 2, 3))");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, FirstParameterCharSlice) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(foreign(123, *i64))");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, NonConstantType) {
  test::TestModule mod;
  mod.AppendCode(R"(
  f := () -> ()
  )");
  auto const *call  = mod.Append<ast::Call>(R"(foreign("f", f))");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, SecondParameterType) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(foreign("f", 3))");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinCallable, Empty) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(callable())");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Interface));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinCallable, ConstantTypes) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(callable(bool))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Interface));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinCallable, NamedTypes) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(callable(b = bool))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Interface));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinCallable, NamedNonConstantTypes) {
  test::TestModule mod;
  mod.AppendCode(R"(
  b := bool
  )");
  auto const *call  = mod.Append<ast::Call>(R"(callable(bool, b = b))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::NonConstant(type::Interface));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinCallable, NonType) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(callable(b = 3))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Interface));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinOpaque, Success) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("opaque()");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinOpaque, Arguments) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("opaque(3)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinBytes, Success) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("bytes(bool)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinBytes, NoArguments) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("bytes()");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinBytes, TooManyArguments) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("bytes(i32, u8)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinBytes, NamedArgument) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("bytes(T = i32)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinBytes, WrongType) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("bytes(3)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinAlignment, Success) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("alignment(bool)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinAlignment, NoArguments) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("alignment()");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinAlignment, TooManyArguments) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("alignment(i32, u8)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinAlignment, NamedArgument) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("alignment(T = i32)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinAlignment, WrongType) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("alignment(3)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(Call, Uncallable) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("3()");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "uncallable-expression")));
}

TEST(Call, CrossModuleCallsWithoutADLGenerateErrors) {
  auto id = ir::ModuleId::New();
  LibraryModule imported_mod;
  imported_mod.set_diagnostic_consumer<diagnostic::TrackingConsumer>();

  test::TestModule mod;
  ON_CALL(mod.importer, Import(Eq("imported")))
      .WillByDefault([id](std::string_view) { return id; });
  ON_CALL(mod.importer, get(id))
      .WillByDefault(
          [&](ir::ModuleId) -> module::BasicModule & { return imported_mod; });

  frontend::SourceBuffer buffer(R"(
  #{export} S ::= struct {}
  #{export} f ::= (s: S) => 3
  )");
  imported_mod.AppendNodes(frontend::Parse(buffer, mod.consumer), mod.consumer,
                           mod.importer);

  mod.AppendCode(R"(
    mod ::= import "imported"
    s: mod.S
    f(s)
  )");
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "undeclared-identifier")));
}

TEST(Call, CrossModuleWithADLSucceed) {
  auto id = ir::ModuleId::New();
  LibraryModule imported_mod;
  imported_mod.set_diagnostic_consumer<diagnostic::TrackingConsumer>();

  test::TestModule mod;
  ON_CALL(mod.importer, Import(Eq("imported")))
      .WillByDefault([id](std::string_view) { return id; });
  ON_CALL(mod.importer, get(id))
      .WillByDefault(
          [&](ir::ModuleId) -> module::BasicModule & { return imported_mod; });

  frontend::SourceBuffer buffer(R"(
  #{export} S ::= struct {}
  #{export} f ::= (s: S) => 3
  )");
  imported_mod.AppendNodes(frontend::Parse(buffer, mod.consumer), mod.consumer,
                           mod.importer);

  mod.AppendCode(R"(
    mod ::= import "imported"
    f ::= (n: i64) => true
    s: mod.S
    s'f
  )");
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Call, CrossModuleWithADLWithoutExport) {
  auto id = ir::ModuleId::New();
  LibraryModule imported_mod;
  imported_mod.set_diagnostic_consumer<diagnostic::TrackingConsumer>();

  test::TestModule mod;
  ON_CALL(mod.importer, Import(Eq("imported")))
      .WillByDefault([id](std::string_view) { return id; });
  ON_CALL(mod.importer, get(id))
      .WillByDefault(
          [&](ir::ModuleId) -> module::BasicModule & { return imported_mod; });

  frontend::SourceBuffer buffer(R"(
  #{export} S ::= struct {}
  f ::= (s: S) => 3
  )");
  imported_mod.AppendNodes(frontend::Parse(buffer, mod.consumer), mod.consumer,
                           mod.importer);

  mod.AppendCode(R"(
    mod ::= import "imported"
    s: mod.S
    s'f
  )");
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "undeclared-identifier")));
}

struct TestCase {
  std::string context;
  std::string expr;
  type::QualType expected_qual_type = type::QualType::Error();
  testing::Matcher<absl::Span<std::pair<std::string, std::string> const>>
      expected_diagnostics = IsEmpty();
};

// TODO: This is starting to get to the point where it should be shared test
// infrastructure.
using CallTest = testing::TestWithParam<TestCase>;
TEST_P(CallTest, Call) {
  auto const &[context, expr, expected_qual_type, expected_diagnostics] =
      GetParam();
  test::TestModule mod;
  mod.AppendCode(context);
  auto const *e     = mod.Append<ast::Expression>(expr);
  auto qts          = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(expected_qual_type));
  EXPECT_THAT(mod.consumer.diagnostics(), expected_diagnostics);
}

INSTANTIATE_TEST_SUITE_P(
    All, CallTest,
    testing::ValuesIn({
        TestCase{
            .context            = "f ::= () => 3",
            .expr               = "f()",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context              = "f ::= () => 3",
            .expr                 = "f(true)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: i64) => n",
            .expr               = "f(4)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context            = "f ::= (n: i64) => n",
            .expr               = "f(n = 4)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context              = "f ::= (n: i64) => 3",
            .expr                 = "f()",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64) => 3",
            .expr                 = "f(b = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64) => 3",
            .expr                 = "f(0, n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 0) => n",
            .expr               = "f()",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 0) => n",
            .expr               = "f(4)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 0) => n",
            .expr               = "f(n = 4)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context              = "f ::= (n: i64 = 0) => 3",
            .expr                 = "f(b = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64 = 0) => 3",
            .expr                 = "f(0, n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: i64, b: bool) => b",
            .expr               = "f(4, true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context              = "f ::= (n: i64, b: bool) => 3",
            .expr                 = "f()",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64, b: bool) => 3",
            .expr                 = "f(0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64, b: bool) => 3",
            .expr                 = "f(true)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64, b: bool) => 3",
            .expr                 = "f(n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64, b: bool) => 3",
            .expr                 = "f(b = true)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64, b: bool) => 3",
            .expr                 = "f(0, n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 3, b: bool = true) => b",
            .expr               = "f(4, true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 3, b: bool = true) => b",
            .expr               = "f(4)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context              = "f ::= (n: i64 = 3, b: bool = true) => 3",
            .expr                 = "f(0, n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 3, b: bool = true) => b",
            .expr               = "f(n = 4)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 3, b: bool = true) => b",
            .expr               = "f(b = true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 3, b: bool = true) => b",
            .expr               = "f(b = true, n = 4)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 3, b: bool = true) => b",
            .expr               = "f()",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        // Overload sets
        TestCase{
            .context            = R"(
            f ::= () => true
            f ::= (n: i64) => n
            )",
            .expr               = "f(0)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context            = R"(
            f ::= () => true
            f ::= (n: i64) => n
            )",
            .expr               = "f()",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = R"(
            f ::= (b: bool) => true
            f ::= (n: i64) => n
            )",
            .expr               = "f(n = 0)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        // Generic functions
        TestCase{
            .context            = "f ::= (x: ~`T) => x",
            .expr               = "f(3)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context            = "f ::= (x: ~`T) => x",
            .expr               = "f(true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context              = "f ::= (x: ~`T) => x",
            .expr                 = "f()",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "S ::= struct (N :: i64) {}",
            .expr               = "S(n :: i64)",
            .expected_qual_type = type::QualType::Constant(type::Interface),
        },
    }));

}  // namespace
}  // namespace compiler
