#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/scope/simple") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(loop { a })", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           Scope(Identifier(), ScopeBodyStart(),
                                 ScopeBlock(ScopeBlockStart(),
                                            StatementSequence(
                                                ScopeStart(),
                                                Statement(StatementStart(),
                                                          Identifier()))))))));
}

NTH_TEST("parser/scope/simple-newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(loop {
    a
  })",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           Scope(Identifier(), ScopeBodyStart(),
                                 ScopeBlock(ScopeBlockStart(),
                                            StatementSequence(
                                                ScopeStart(),
                                                Statement(StatementStart(),
                                                          Identifier()))))))));
}

NTH_TEST("parser/scope-literal/simple") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(scope [ctx] { a })", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(
                     ScopeStart(),
                     Statement(StatementStart(),
                               ScopeLiteral(ScopeLiteralStart(), Identifier(),
                                            StatementSequence(
                                                ScopeStart(),
                                                Statement(StatementStart(),
                                                          Identifier())))))));
}

}  // namespace
}  // namespace ic
