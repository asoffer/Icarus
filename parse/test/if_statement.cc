#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/if-statement/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {}
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        IfStatement(Identifier(), IfStatementTrueBranchStart(),
                                    StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/if-statement/empty-newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        IfStatement(Identifier(), IfStatementTrueBranchStart(),
                                    StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/if-statement/with-one-line-body") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
    body
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  IfStatement(Identifier(), IfStatementTrueBranchStart(),
                              StatementSequence(ScopeStart(),
                                                Statement(StatementStart(),
                                                          Identifier())))))));
}

NTH_TEST("parser/if-statement/with-body") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
    body
    body
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        IfStatement(
                            Identifier(), IfStatementTrueBranchStart(),
                            StatementSequence(
                                ScopeStart(),
                                Statement(StatementStart(), Identifier()),
                                Statement(StatementStart(), Identifier())))))));
}

NTH_TEST("parser/if-else-statement/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
  } else {
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        IfStatement(Identifier(), IfStatementTrueBranchStart(),
                                    StatementSequence(ScopeStart()),
                                    IfStatementFalseBranchStart(),
                                    StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/if-else-statement/with-else-body") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
  } else {
    body
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  IfStatement(Identifier(), IfStatementTrueBranchStart(),
                              StatementSequence(ScopeStart()),
                              IfStatementFalseBranchStart(),
                              StatementSequence(ScopeStart(),
                                                Statement(StatementStart(),
                                                          Identifier())))))));
}

NTH_TEST("parser/if-else-statement/with-bodies") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {
    body
  } else {
    body
  }
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  IfStatement(Identifier(), IfStatementTrueBranchStart(),
                              StatementSequence(
                                  ScopeStart(),
                                  Statement(StatementStart(), Identifier())),
                              IfStatementFalseBranchStart(),
                              StatementSequence(ScopeStart(),
                                                Statement(StatementStart(),
                                                          Identifier())))))));
}

NTH_TEST("parser/if-else-statement/one-line-with-bodies") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(if (condition) { body } else { body })", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  IfStatement(Identifier(), IfStatementTrueBranchStart(),
                              StatementSequence(
                                  ScopeStart(),
                                  Statement(StatementStart(), Identifier())),
                              IfStatementFalseBranchStart(),
                              StatementSequence(ScopeStart(),
                                                Statement(StatementStart(),
                                                          Identifier())))))));
}

NTH_TEST("parser/if-statement/else-if") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  if (condition) {} else if (condition) {}
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(
                     StatementStart(),
                     IfStatement(
                         Identifier(), IfStatementTrueBranchStart(),
                         StatementSequence(ScopeStart()),
                         IfStatementFalseBranchStart(),
                         StatementSequence(
                             ScopeStart(),
                             Statement(
                                 StatementStart(),
                                 IfStatement(
                                     Identifier(), IfStatementTrueBranchStart(),
                                     StatementSequence(ScopeStart())))))))));
}

}  // namespace
}  // namespace ic
