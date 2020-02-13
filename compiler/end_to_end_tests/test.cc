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

TEST(DesignatedInitializer, Empty) {
  std::string output = RunProgram(R"(
  S ::= struct {}
  s := S.{}
  )");

  EXPECT_EQ(output, "");
}


TEST(DesignatedInitializer, OneFieldBool) {
  std::string output = RunProgram(R"(
  S ::= struct { b: bool }
  s1 := S.{ b = true }
  s2 := S.{}
  print s1.b, " ", s2.b
  )");
  EXPECT_EQ(output, "true false");
}

TEST(DesignatedInitializer, OneFieldInt64) {
  std::string output = RunProgram(R"(
  S ::= struct { n: int64 }
  s1 := S.{ n = 2 }
  s2 := S.{}
  print s1.n, " ", s2.n
  )");
  EXPECT_EQ(output, "2 0");
}

TEST(DesignatedInitializer, MultipleFields) {
  std::string output = RunProgram(R"(
  S ::= struct { n: int64 \\ b: bool }
  s1 := S.{ n = 2 \\ b = true }
  s2 := S.{ n = 2 }
  s3 := S.{ b = true }
  s4 := S.{}
  print s1.n, s1.b, " ", s2.n, s2.b, " ", s3.n, s3.b, " ", s4.n, s4.b
  )");
  EXPECT_EQ(output, "2true 2false 0true 0false");

  output = RunProgram(R"(
  S ::= struct { b: bool \\ n: int64 }
  s1 := S.{ n = 2 \\ b = true }
  s2 := S.{ n = 2 }
  s3 := S.{ b = true }
  s4 := S.{}
  print s1.b, s1.n, " ", s2.b, s2.n, " ", s3.b, s3.n, " ", s4.b, s4.n
  )");
  EXPECT_EQ(output, "true2 false2 true0 false0");
}

TEST(DesignatedInitializer, Nested) {
  std::string output = RunProgram(R"(
  S ::= struct { n: int64 \\ b: bool }
  T ::= struct { n: int64 \\ b: bool \\ s: S }
  t1 := T.{}
  t2 := T.{ n = 1 \\ b = true \\ s = S.{} }
  print t1.n, t1.b, t1.s.n, t1.s.b, " ",
        t2.n, t2.b, t2.s.n, t2.s.b
  )");
  EXPECT_EQ(output, "0false0false 1true0false");
}


}  // namespace
