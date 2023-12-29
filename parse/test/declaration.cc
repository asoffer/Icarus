#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/declaration/integer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x ::= 3", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(),
                                             Declaration(DeclarationStart(),
                                                         DeclaredIdentifier(),
                                                         IntegerLiteral())))));
}

NTH_TEST("parser/declaration/bool") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x := true", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(),
                                             Declaration(DeclarationStart(),
                                                         DeclaredIdentifier(),
                                                         BooleanLiteral())))));
}

NTH_TEST("parser/declaration/with-type-and-initializer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x: T = true", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           Declaration(DeclarationStart(), DeclaredIdentifier(),
                                       Identifier(), BooleanLiteral())))));
}

NTH_TEST("parser/declaration/with-type-but-no-initializer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x: T", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(),
                                             Declaration(DeclarationStart(),
                                                         DeclaredIdentifier(),
                                                         Identifier())))));
}

NTH_TEST("parser/declaration/with-comment") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x ::= true  // comment!", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(),
                                             Declaration(DeclarationStart(),
                                                         DeclaredIdentifier(),
                                                         BooleanLiteral())))));
}

NTH_TEST("parser/multiple-declarations-with-newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  let x ::= 3
  var y ::= 4
  )",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           Declaration(DeclarationStart(), DeclaredIdentifier(),
                                       IntegerLiteral())),
                 Statement(StatementStart(),
                           Declaration(DeclarationStart(), DeclaredIdentifier(),
                                       IntegerLiteral())))));
}

}  // namespace
}  // namespace ic
