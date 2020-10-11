#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct TestCase {
  std::string expr;
  ir::Value expected;
};

using ScopeNodeTest = testing::TestWithParam<TestCase>;
TEST_P(ScopeNodeTest, ScopeNode) {
  auto const &[ expr, expected] = GetParam();
  test::TestModule mod;
  // TODO: We can't use `s` as the field member because the compiler thinks
  // there's an ambiguity (there isn't).
  auto const *e  = mod.Append<ast::Expression>(expr);
  auto const *qt = mod.data().qual_type(e);
  ASSERT_NE(qt, nullptr);
  auto const *t = qt->type();
  ASSERT_NE(t, nullptr);
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, expected);
}

// Note: We test both with literals and with a unary-operator applied directly
// to a function call. The former helps cover the constant-folding mechanisms
// built in to the ir::Builder. The latter helps cover the common case for code
// emission.
INSTANTIATE_TEST_SUITE_P(All, ScopeNodeTest,
                         testing::ValuesIn({
                             TestCase{
                                 .expr     = R"((() -> int64 {
  n := 0
  ignore ::= scope {
    enter ::= jump() { goto done() }
    body ::= block {
      before ::= () -> () {}
      after ::= jump() { goto done()  }
    }
    exit ::= () -> () {}
  }
  ignore () body { n = 1 }
  return n
})()
                                  )",
                                 .expected = ir::Value(int64_t{0}),
                             },
                             TestCase{
                                 .expr     = R"((() -> int64 {
  just ::= scope {
    enter ::= jump() { goto do() }
    do ::= block {
      before ::= () -> () {}
      after ::= jump() { goto done()  }
    }
    exit ::= () -> () {}
  }

  n := 0
  just () do {
    n = 1
  }
  return n
})()
                             )",
                                 .expected = ir::Value(int64_t{1}),
                             },
                             TestCase{
                                 .expr     = R"((() -> int64 {
  while ::= scope {
    enter ::= jump(b: bool) { goto b, do(), done() }
    do ::= block {
      before ::= () -> () {}
      after ::= jump() { goto start()  }
    }
    exit ::= () -> () {}
  }

  n := 0
  while (n < 10) do {
    n += 3
  }
  return n
})()
                             )",
                                 .expected = ir::Value(int64_t{12}),
                             },

                             TestCase{
                                 .expr     = R"((() -> int64 {
  if ::= scope {
    enter ::= jump(condition: bool) { goto condition, then(), else() | done() }
    then ::= block {
      before ::= () -> () {}
      after ::= jump() { goto done()  }
    }
    else ::= block {
      before ::= () -> () {}
      after ::= jump() { goto done()  }
    }
    exit ::= () -> () {}
  }

  a := 0
  b := 0
  c := 0
  d := 0
  if (a == 0) then { a = 1 }
  if (b != 0) then { b = 1 }
  if (c == 0) then {} else { c = 1 }
  if (d != 0) then {} else { d = 1 }
  return 1000 * a + 100 * b + 10 * c + d
})()
                             )",
                                 .expected = ir::Value(int64_t{1001}),
                             },
                         }));

// TODO: Stateful scopes
// TODO: Ensure `before()` gets called.
// TODO: Ensure destructors run
// TODO: Quick exiting.
// TODO: Yield statements.
// TODO: Nested yields.
// TODO: Parameters and arguments.

}  // namespace
}  // namespace compiler