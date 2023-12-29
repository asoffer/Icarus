#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/enum/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(enum {})", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(
                     ScopeStart(),
                     Statement(StatementStart(), EnumLiteral(EnumLiteralStart(),
                                                             ScopeStart())))));
}

NTH_TEST("parser/enum/singleton") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(enum { a })", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  EnumLiteral(EnumLiteralStart(), ScopeStart(),
                                              DeclaredIdentifier())))));
}

NTH_TEST("parser/enum/singleton-with-newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(enum {
    a
  })",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  EnumLiteral(EnumLiteralStart(), ScopeStart(),
                                              DeclaredIdentifier())))));
}

NTH_TEST("parser/enum/multiple") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(enum {
    a
    b
    c
  })",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        EnumLiteral(EnumLiteralStart(), ScopeStart(),
                                    DeclaredIdentifier(), DeclaredIdentifier(),
                                    DeclaredIdentifier())))));
}

}  // namespace
}  // namespace ic
