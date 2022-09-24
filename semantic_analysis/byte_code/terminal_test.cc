#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::EvaluatesTo;

TEST(Terminal, Evaluation) {
  test::Repl repl;
  auto& type_system = repl.type_system();

  EXPECT_THAT(repl.execute("true"), EvaluatesTo(true));
  EXPECT_THAT(repl.execute("false"), EvaluatesTo(false));
  EXPECT_THAT(repl.execute("bool"), EvaluatesTo(Bool));
  EXPECT_THAT(repl.execute("char"), EvaluatesTo(Char));
  EXPECT_THAT(repl.execute("i8"),
              EvaluatesTo(core::Type(core::SizedIntegerType::I<8>())));
  EXPECT_THAT(repl.execute("i16"),
              EvaluatesTo(core::Type(core::SizedIntegerType::I<16>())));
  EXPECT_THAT(repl.execute("i32"),
              EvaluatesTo(core::Type(core::SizedIntegerType::I<32>())));
  EXPECT_THAT(repl.execute("i64"),
              EvaluatesTo(core::Type(core::SizedIntegerType::I<64>())));
  EXPECT_THAT(repl.execute("u8"),
              EvaluatesTo(core::Type(core::SizedIntegerType::U<8>())));
  EXPECT_THAT(repl.execute("u16"),
              EvaluatesTo(core::Type(core::SizedIntegerType::U<16>())));
  EXPECT_THAT(repl.execute("u32"),
              EvaluatesTo(core::Type(core::SizedIntegerType::U<32>())));
  EXPECT_THAT(repl.execute("u64"),
              EvaluatesTo(core::Type(core::SizedIntegerType::U<64>())));
}

}  // namespace
}  // namespace semantic_analysis

