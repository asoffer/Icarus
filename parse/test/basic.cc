#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/empty", std::string_view content) {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(content, d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(), StatementSequence(ScopeStart())));
}

NTH_INVOKE_TEST("parser/empty") {
  co_yield "";
  co_yield "\n";
  co_yield "\n\n";
  co_yield "\n   \n";
  co_yield "// comment!";
}

NTH_TEST("parser/parenthesized") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"((a))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(), Identifier()))));
}

NTH_TEST("parser/parenthesized/newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"((
  a))",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(), Identifier()))));
}

}  // namespace
}  // namespace ic
