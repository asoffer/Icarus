#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/access/basic") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(),
                                             MemberExpression(Identifier())))));
}

NTH_TEST("parser/access/nested") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b.c)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           MemberExpression(MemberExpression(Identifier()))))));
}

NTH_TEST("parser/access/precedence") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b * c.d)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           ExpressionPrecedenceGroup(
                               MemberExpression(Identifier()), InfixOperator(),
                               MemberExpression(Identifier()))))));
}

NTH_TEST("parser/pointer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(*a)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(ScopeStart(),
                            Statement(StatementStart(),
                                      Pointer(PointerStart(), Identifier())))));
}

NTH_TEST("parser/pointer/access") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(*a.b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  Pointer(PointerStart(),
                                          MemberExpression(Identifier()))))));
}

}  // namespace
}  // namespace ic
