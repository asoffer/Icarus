#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/interface/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(interface [T] {})", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        InterfaceLiteral(InterfaceLiteralStart(),
                                         StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/interface/declaration") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(interface [T] {
    let x ::= T
  })",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           InterfaceLiteral(
                               InterfaceLiteralStart(),
                               StatementSequence(
                                   ScopeStart(),
                                   Statement(StatementStart(),
                                             Declaration(DeclarationStart(),
                                                         DeclaredIdentifier(),
                                                         Identifier()))))))));
}

NTH_TEST("parser/interface/multiple-declaration") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(interface [T] {
    let x ::= T
    let y ::= T
  })",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           InterfaceLiteral(
                               InterfaceLiteralStart(),
                               StatementSequence(
                                   ScopeStart(),
                                   Statement(StatementStart(),
                                             Declaration(DeclarationStart(),
                                                         DeclaredIdentifier(),
                                                         Identifier())),
                                   Statement(StatementStart(),
                                             Declaration(DeclarationStart(),
                                                         DeclaredIdentifier(),
                                                         Identifier()))))))));
}

}  // namespace
}  // namespace ic
