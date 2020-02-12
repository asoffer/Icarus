#include "compiler/executable_module.h"
#include "frontend/source/string.h"
#include "gtest/gtest.h"
#include "interpretter/execute.h"
#include "test/failing_diagnostic_consumer.h"

std::string RunProgram(std::string str) {
  str += R"(
  if ::= scope {
    init ::= jump(b: bool) {
      switch (b) {
        goto then() when true
        goto else() | exit() when false
      }
    }
    then ::= block {
      before ::= () -> () {}
      after ::= jump() { goto exit() }
    }
  
    else ::= block {
      before ::= () -> () {}
      after ::= jump() { goto exit() }
    }
  
    done ::= () -> () {}
  }
  )";

  frontend::StringSource src(std::move(str));
  test::FailingConsumer diag;

  compiler::ExecutableModule mod;
  mod.ProcessFromSource(&src, diag);
  mod.main()->WriteByteCode();
  std::stringstream ss;
  interpretter::ExecutionContext exec_ctx(ss);
  interpretter::Execute(
      mod.main(), base::untyped_buffer::MakeFull(mod.main()->num_regs() * 16),
      {}, &exec_ctx);
  return ss.str();
}

namespace {

TEST(Test, Fibonacci) {
  std::string output = RunProgram(R"(
  print f(10)
  
  f ::= (n: int64) -> int64 {
    if (n < 2) then { return n }
    return f(n - 1) + f(n - 2)
  })");
  EXPECT_EQ(output, "55");
}

TEST(Test, Factorial) {
  std::string output = RunProgram(R"(
  print f(6)
  
  f ::= (n: int64) -> int64 {
    if (n < 2) then { return 1 }
    return n * f(n - 1)
  })");
  EXPECT_EQ(output, "720");
}

}  // namespace
