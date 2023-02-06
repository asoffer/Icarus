#include "gtest/gtest.h"
#include "data_types/module_id.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(Access, Evaluation) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<uint64_t>(R"("abc".length)"), 3);
  char const *ptr    = repl.execute<char const *>(R"("abc".data)");
  EXPECT_EQ(ptr, repl.ast_module()
                     .stmts()
                     .back()
                     ->as<ast::Access>()
                     .operand()
                     ->as<ast::Terminal>()
                     .value<std::string>()
                     .data());
}

TEST(Access, ReferenceEvaluation) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<uint64_t>(R"(((s: [/]char) -> u64 {
    return s.length
  })("abc"))"),
            3);
  // char const *ptr = repl.execute<char const *>(R"(((s: [/]char) -> [*]char {
  //   return s.data
  // })("abc"))");
  // EXPECT_EQ(std::string(ptr, 3), "abc");
}

}  // namespace
}  // namespace semantic_analysis
