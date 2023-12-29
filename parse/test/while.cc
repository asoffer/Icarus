#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/while/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(while (condition) {})", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(
                     ScopeStart(),
                     Statement(StatementStart(),
                               WhileLoop(WhileLoopStart(), Identifier(),
                                         WhileLoopBodyStart(),
                                         StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/while/statements") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(while (condition) {
    a
    b
  })",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(
                     StatementStart(),
                     WhileLoop(
                         WhileLoopStart(), Identifier(), WhileLoopBodyStart(),
                         StatementSequence(
                             ScopeStart(),
                             Statement(StatementStart(), Identifier()),
                             Statement(StatementStart(), Identifier())))))));
}

}  // namespace
}  // namespace ic
