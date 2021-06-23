#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using Test = test::EvaluationTest;
INSTANTIATE_TEST_SUITE_P(
    All, Test,
    testing::ValuesIn({
        test::TestCase{
            .expr     = R"((() -> i64 {
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
        test::TestCase{
            .expr     = R"((() -> i64 {
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
        test::TestCase{
            .expr     = R"((() -> i64 {
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

        test::TestCase{
            .expr     = R"((() -> i64 {
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

        // Early return
        test::TestCase{
            .expr     = R"((() -> i64 {
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

  func ::= (b: bool, n: *i64) -> i64 {
    if (b) then {
      @n = 1
      return 0
    } else {
      @n = 2
    }
    @n = 3
    return 0
  }

  a := 0
  b := 0
  func(true, &a)
  func(false, &b)
  return 10 * b + a
})()
                             )",
            .expected = ir::Value(int64_t{31}),
        },
        test::TestCase{
            .expr     = R"((() -> i64 {
  s ::= scope {
    enter ::= jump() { goto do() }
    do ::= block {
      before ::= () -> () {}
      after ::= jump() { goto done() }
    }
    exit ::= (n: i64) => n
  }

  return #.l s () do {
    #.l << 3
  }
})()
                             )",
            .expected = ir::Value(int64_t{3}),
        },
        test::TestCase{
            .expr     = R"((() -> i64 {
  repeat ::= scope (i64) {
    enter ::= jump [state: *i64] (n: i64) {
      @state = n
      goto @state == 0, done(), do() 
    }
  
    do ::= block {
      before ::= () -> () {}
      after ::= jump [state: *i64] () { 
        @state -= 1
        goto @state == 0, done(), do()
      }
    }
  
    exit ::= () -> () {}
  }

  num := 1
  repeat (10) do { num *= 2 }
  return num
})()
                             )",
            .expected = ir::Value(int64_t{1024}),
        },
        test::TestCase{
            .expr     = R"((() -> i64 {
  repeat ::= scope (i64) {
    enter ::= jump [state: *i64] (n: i64) {
      @state = n
      goto @state == 0, done(), do() 
    }
  
    do ::= block {
      before ::= () -> () {}
      after ::= jump [state: *i64] () { 
        @state -= 1
        goto @state == 0, done(), do()
      }
    }
  
    exit ::= () -> () {}
  }

  num := 1
  repeat (10) do {
    num *= 2 
    return num
  }
  return num
})()
                             )",
            .expected = ir::Value(int64_t{2}),
        },
        test::TestCase{
            .expr = R"((() -> i64 {
  scope_with_big_argument ::= scope {
    enter ::= jump (str: []char) { goto done(str) }
    exit ::= (str: []char) -> () {}
  
    do ::= block {
      before ::= () -> () {}
      after ::= jump () { goto do() }
    }
  }

  scope_with_big_argument ("abc") do {}
  return 0
})()
                             )",
            // Really just checking that this compiles with a value
            .expected = ir::Value(int64_t{0}),
        },

    }));

// TODO: Ensure `before()` gets called.
// TODO: Ensure destructors run
// TODO: Quick exiting.
// TODO: Nested yields.
// TODO: Parameters and arguments.

}  // namespace
}  // namespace compiler
