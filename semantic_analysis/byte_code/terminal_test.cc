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
  EXPECT_THAT(repl.execute("i8"),
              EvaluatesTo(core::Type(core::BuiltinType::I<8>(type_system))));
  EXPECT_THAT(repl.execute("i16"),
              EvaluatesTo(core::Type(core::BuiltinType::I<16>(type_system))));
  EXPECT_THAT(repl.execute("i32"),
              EvaluatesTo(core::Type(core::BuiltinType::I<32>(type_system))));
  EXPECT_THAT(repl.execute("i64"),
              EvaluatesTo(core::Type(core::BuiltinType::I<64>(type_system))));
  EXPECT_THAT(repl.execute("u8"),
              EvaluatesTo(core::Type(core::BuiltinType::U<8>(type_system))));
  EXPECT_THAT(repl.execute("u16"),
              EvaluatesTo(core::Type(core::BuiltinType::U<16>(type_system))));
  EXPECT_THAT(repl.execute("u32"),
              EvaluatesTo(core::Type(core::BuiltinType::U<32>(type_system))));
  EXPECT_THAT(repl.execute("u64"),
              EvaluatesTo(core::Type(core::BuiltinType::U<64>(type_system))));
}

}  // namespace
}  // namespace semantic_analysis

