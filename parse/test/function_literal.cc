#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/function-literal/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(fn() -> x {})", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        FunctionLiteral(FunctionLiteralStart(),
                                        FunctionLiteralSignature(Identifier()),
                                        StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/function-literal/one-parameter") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(fn(let a: b) -> x {})", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        FunctionLiteral(
                            FunctionLiteralStart(),
                            FunctionLiteralSignature(
                                Declaration(DeclarationStart(),
                                            DeclaredIdentifier(), Identifier()),
                                Identifier()),
                            StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/function-literal/multiple-parameters") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
    fn(let a: b,
       let c: d) -> x {})",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        FunctionLiteral(
                            FunctionLiteralStart(),
                            FunctionLiteralSignature(
                                Declaration(DeclarationStart(),
                                            DeclaredIdentifier(), Identifier()),
                                Declaration(DeclarationStart(),
                                            DeclaredIdentifier(), Identifier()),
                                Identifier()),
                            StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/function-literal/body") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
    fn(let a: b) -> x {
      y
    })",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        FunctionLiteral(
                            FunctionLiteralStart(),
                            FunctionLiteralSignature(
                                Declaration(DeclarationStart(),
                                            DeclaredIdentifier(), Identifier()),
                                Identifier()),
                            StatementSequence(
                                ScopeStart(),
                                Statement(StatementStart(), Identifier())))))));
}

NTH_TEST("parser/function-literal/no-returns-with-newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  fn() -> (
    // No returns here!
  ) {})",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        FunctionLiteral(FunctionLiteralStart(),
                                        FunctionLiteralSignature(NoReturns()),
                                        StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/function-literal/no-returns") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(fn() -> () {})", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        FunctionLiteral(FunctionLiteralStart(),
                                        FunctionLiteralSignature(NoReturns()),
                                        StatementSequence(ScopeStart()))))));
}

NTH_TEST("parser/return-expression") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(return a)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(ScopeStart(), Statement(StatementStart(),
                                                       Return(Identifier())))));
}

NTH_TEST("parser/function-literal/return-expression") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
    fn(let a: b) -> x {
      return c
    })",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  FunctionLiteral(
                      FunctionLiteralStart(),
                      FunctionLiteralSignature(
                          Declaration(DeclarationStart(), DeclaredIdentifier(),
                                      Identifier()),
                          Identifier()),
                      StatementSequence(ScopeStart(),
                                        Statement(StatementStart(),
                                                  Return(Identifier()))))))));
}

}  // namespace
}  // namespace ic
