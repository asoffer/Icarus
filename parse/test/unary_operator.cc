#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "nth/test/test.h"
#include "parse/parser.h"
#include "parse/test/matchers.h"
#include "parse/test/tree_node_ref.h"

namespace ic {
namespace {

NTH_TEST("parser/pointer/function") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(*a -> b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(ScopeStart(),
                            Statement(StatementStart(),
                                      ExpressionPrecedenceGroup(
                                          Pointer(PointerStart(), Identifier()),
                                          InfixOperator(), Identifier())))));
}

NTH_TEST("parser/buffer-pointer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"([*]a)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>= Module(
                 ModuleStart(),
                 StatementSequence(ScopeStart(),
                                   Statement(StatementStart(),
                                             BufferPointer(BufferPointerStart(),
                                                           Identifier())))));
}

NTH_TEST("parser/buffer-pointer/access") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"([*]a.b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           BufferPointer(BufferPointerStart(),
                                         MemberExpression(Identifier()))))));
}

NTH_TEST("parser/buffer-pointer/function") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"([*]a -> b)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>= Module(
          ModuleStart(),
          StatementSequence(
              ScopeStart(),
              Statement(StatementStart(),
                        ExpressionPrecedenceGroup(
                            BufferPointer(BufferPointerStart(), Identifier()),
                            InfixOperator(), Identifier())))));
}

NTH_TEST("parser/slice/once") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(\i32)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(ScopeStart(),
                               Statement(StatementStart(),
                                         Slice(SliceStart(), TypeLiteral())))));
}

NTH_TEST("parser/slice/multiple") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(\\\i32)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(
      FromRoot(tree) >>=
      Module(ModuleStart(),
             StatementSequence(
                 ScopeStart(),
                 Statement(StatementStart(),
                           Slice(SliceStart(),
                                 Slice(SliceStart(),
                                       Slice(SliceStart(), TypeLiteral())))))));
}

NTH_TEST("parser/address") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(&n + 0)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  ExpressionPrecedenceGroup(
                                      Address(AddressStart(), Identifier()),
                                      InfixOperator(), IntegerLiteral())))));
}

NTH_TEST("parser/deref") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(@n + 0)", d);
  auto tree          = Parse(buffer, d).parse_tree;
  NTH_EXPECT(FromRoot(tree) >>=
             Module(ModuleStart(),
                    StatementSequence(
                        ScopeStart(),
                        Statement(StatementStart(),
                                  ExpressionPrecedenceGroup(
                                      Deref(DerefStart(), Identifier()),
                                      InfixOperator(), IntegerLiteral())))));
}

}  // namespace
}  // namespace ic
