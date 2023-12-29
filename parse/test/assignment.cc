#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/assignment") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a = b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  Assignment(Identifier(), AssignedValueStart(),
                                             Identifier())))));
}

NTH_TEST("parser/assignment/with-operators") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a + b = c + d)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  Assignment(ExpressionPrecedenceGroup(
                                                 Identifier(), InfixOperator(),
                                                 Identifier()),
                                             AssignedValueStart(),
                                             ExpressionPrecedenceGroup(
                                                 Identifier(), InfixOperator(),
                                                 Identifier()))))));
}

NTH_TEST("parser/assignment/complex") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b = c.d + e)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           Assignment(MemberExpression(Identifier()),
                                      AssignedValueStart(),
                                      ExpressionPrecedenceGroup(
                                          MemberExpression(Identifier()),
                                          InfixOperator(), Identifier()))))));
}

}  // namespace
}  // namespace ic
