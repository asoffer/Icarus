#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/invoke/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(f())", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  CallExpression(Identifier(),
                                                 InvocationArgumentStart())))));
}

NTH_TEST("parser/invoke/empty-member-call") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b())", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  CallExpression(MemberExpression(Identifier()),
                                                 InvocationArgumentStart())))));
}

NTH_TEST("parser/invoke/double-call") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b()())", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  CallExpression(CallExpression(MemberExpression(Identifier()),
                                                InvocationArgumentStart()),
                                 InvocationArgumentStart())))));
}

NTH_TEST("parser/invoke/one-positional") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a(b))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        CallExpression(Identifier(), InvocationArgumentStart(),
                                       Identifier())))));
}

NTH_TEST("parser/invoke/one-positional-newline") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a(
  b
  ))",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        CallExpression(Identifier(), InvocationArgumentStart(),
                                       Identifier())))));
}

NTH_TEST("parser/invoke/multiple-positional") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a(b, c, d))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  CallExpression(Identifier(), InvocationArgumentStart(),
                                 Identifier(), Identifier(), Identifier())))));
}

NTH_TEST("parser/invoke/multiple-positional-newline") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  a(b, c,
    d))",
                                d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(
                  StatementStart(),
                  CallExpression(Identifier(), InvocationArgumentStart(),
                                 Identifier(), Identifier(), Identifier())))));
}

NTH_TEST("parser/invoke/access-call") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b(c.d))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           CallExpression(MemberExpression(Identifier()),
                                          InvocationArgumentStart(),
                                          MemberExpression(Identifier()))))));
}


NTH_TEST("parser/invoke/prefix-empty-suffix") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a'f())", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(
                     ScopeStart(),
                     Statement(StatementStart(),
                               CallExpression(
                                   Identifier(), PrefixInvocationArgumentEnd(),
                                   Identifier(), InvocationArgumentStart())))));
}

NTH_TEST("parser/invoke/prefix-nonempty-suffix") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a'f(b, c))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(
                     ScopeStart(),
                     Statement(StatementStart(),
                               CallExpression(
                                   Identifier(), PrefixInvocationArgumentEnd(),
                                   Identifier(), InvocationArgumentStart(),
                                   Identifier(), Identifier())))));
}

NTH_TEST("parser/invoke/named-argument") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(f(a = b))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        CallExpression(Identifier(), InvocationArgumentStart(),
                                       NamedArgument(NamedArgumentStart(),
                                                     Identifier()))))));
}

NTH_TEST("parser/invoke/positional-and-named-arguments") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(f(a, b = c, d = e))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(
                     StatementStart(),
                     CallExpression(
                         Identifier(), InvocationArgumentStart(), Identifier(),
                         NamedArgument(NamedArgumentStart(), Identifier()),
                         NamedArgument(NamedArgumentStart(), Identifier()))))));
}

NTH_TEST("parser/invoke/prefix-call-with-postfix-named-arguments") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a'f(b = c, d = e))", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(
                     StatementStart(),
                     CallExpression(
                         Identifier(), PrefixInvocationArgumentEnd(),
                         Identifier(), InvocationArgumentStart(),
                         NamedArgument(NamedArgumentStart(), Identifier()),
                         NamedArgument(NamedArgumentStart(), Identifier()))))));
}

}  // namespace
}  // namespace ic
