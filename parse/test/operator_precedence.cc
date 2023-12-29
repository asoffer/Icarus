#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/operator-precedence/plus-times") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(x + y * z)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(), ExpressionPrecedenceGroup(
                                              Identifier(), InfixOperator(),
                                              ExpressionPrecedenceGroup(
                                                  Identifier(), InfixOperator(),
                                                  Identifier()))))));
}

NTH_TEST("parser/operator-precedence/times-plus") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(x * y + z)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           ExpressionPrecedenceGroup(
                               ExpressionPrecedenceGroup(
                                   Identifier(), InfixOperator(), Identifier()),
                               InfixOperator(), Identifier())))));
}

NTH_TEST("parser/operator-precedence/multiple-levels") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a + b * c == d)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(ScopeStart(),
                            Statement(StatementStart(),
                                      ExpressionPrecedenceGroup(
                                          ExpressionPrecedenceGroup(
                                              Identifier(), InfixOperator(),
                                              ExpressionPrecedenceGroup(
                                                  Identifier(), InfixOperator(),
                                                  Identifier())),
                                          InfixOperator(), Identifier())))));
}

NTH_TEST("parser/operator-precedence/multiple-levels-balanced") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a + b == c * d)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(
                     StatementStart(),
                     ExpressionPrecedenceGroup(
                         ExpressionPrecedenceGroup(
                             Identifier(), InfixOperator(), Identifier()),
                         InfixOperator(),
                         ExpressionPrecedenceGroup(
                             Identifier(), InfixOperator(), Identifier()))))));
}

NTH_TEST("parser/operator-precedence/plus-plus") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(x + y + z)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(
                     ScopeStart(),
                     Statement(StatementStart(),
                               ExpressionPrecedenceGroup(
                                   Identifier(), InfixOperator(), Identifier(),
                                   InfixOperator(), Identifier())))));
}

}  // namespace
}  // namespace ic
