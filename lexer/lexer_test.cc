#include "lexer/lexer.h"

#include "diagnostics/consumer/null.h"
#include "lexer/token_matchers.h"
#include "nth/test/test.h"

namespace ic {
namespace {

using ::ic::testing::HasImmediateIntegerValue;
using ::ic::testing::HasKind;
using ::nth::ElementsAreSequentially;

NTH_TEST("lex/empty") {
  diag::NullConsumer d;
  auto token_buffer = Lex("", d);
  NTH_EXPECT(token_buffer.size() == 1);
}

NTH_TEST("lex/keyword") {
  diag::NullConsumer d;
  auto token_buffer = Lex("let", d);
  NTH_EXPECT(token_buffer.size() == 2) NTH_ELSE { return; }
  NTH_EXPECT(token_buffer[0].kind() == Token::Kind::Let);

  token_buffer = Lex("var", d);
  NTH_EXPECT(token_buffer.size() == 2) NTH_ELSE { return; }
  NTH_EXPECT(token_buffer[0].kind() == Token::Kind::Var);

  token_buffer = Lex("true", d);
  NTH_EXPECT(token_buffer.size() == 2) NTH_ELSE { return; }
  NTH_EXPECT(token_buffer[0].kind() == Token::Kind::True);

  token_buffer = Lex("false", d);
  NTH_EXPECT(token_buffer.size() == 2) NTH_ELSE { return; }
  NTH_EXPECT(token_buffer[0].kind() == Token::Kind::False);
}

NTH_TEST("lex/identifier", std::string_view id) {
  diag::NullConsumer d;
  auto token_buffer = Lex(id, d);
  NTH_EXPECT(token_buffer.size() == 2) NTH_ELSE { return; }
  NTH_EXPECT(token_buffer[0].kind() == Token::Kind::Identifier);
}

NTH_INVOKE_TEST("lex/identifier") {
  for (std::string_view id :
       {"a", "name", "_blah", "_17", "blah__", "blah_17_", "___"}) {
    co_yield id;
  }
}

NTH_TEST("lex/integer", std::string_view n, uint32_t expected) {
  diag::NullConsumer d;
  auto token_buffer = Lex(n, d);
  NTH_EXPECT(token_buffer >>=
             ElementsAreSequentially(HasImmediateIntegerValue(expected),
                                     HasKind(Token::Kind::Eof)));
}

NTH_INVOKE_TEST("lex/integer") {
  co_yield nth::TestArguments{"0", 0};
  co_yield nth::TestArguments{"0d0", 0};
  co_yield nth::TestArguments{"123", 123};
  co_yield nth::TestArguments{"0d123", 123};
}

NTH_TEST("lex/basic") {
  diag::NullConsumer d;
  auto token_buffer = Lex("let x ::= 3", d);
  NTH_EXPECT(token_buffer >>= ElementsAreSequentially(
                 HasKind(Token::Kind::Let), HasKind(Token::Kind::Identifier),
                 HasKind(Token::Kind::ColonColonEqual),
                 HasImmediateIntegerValue(3), HasKind(Token::Kind::Eof)));
}

NTH_TEST("lex/comment/eof") {
  diag::NullConsumer d;
  auto token_buffer = Lex("// comment", d);
  NTH_EXPECT(token_buffer >>= ElementsAreSequentially(HasKind(Token::Kind::Eof)));
}

NTH_TEST("lex/comment/multiple") {
  diag::NullConsumer d;
  auto token_buffer = Lex(
      R"(// some comments
         // more comments)",
      d);
  NTH_EXPECT(token_buffer >>= ElementsAreSequentially(
                 HasKind(Token::Kind::Newline), HasKind(Token::Kind::Eof)));
}

NTH_TEST("lex/comment/with-non-comments") {
  diag::NullConsumer d;
  auto token_buffer = Lex(
      R"(// some comments
         x
         // more comments)",
      d);
  NTH_EXPECT(token_buffer >>= ElementsAreSequentially(
                 HasKind(Token::Kind::Newline),
                 HasKind(Token::Kind::Identifier),
                 HasKind(Token::Kind::Newline), HasKind(Token::Kind::Eof)));
}

NTH_TEST("lex/comment/end-of-line") {
  diag::NullConsumer d;
  auto token_buffer = Lex(R"(x // some comments)", d);
  NTH_EXPECT(token_buffer >>= ElementsAreSequentially(
                 HasKind(Token::Kind::Identifier), HasKind(Token::Kind::Eof)));
}

}  // namespace
}  // namespace ic
