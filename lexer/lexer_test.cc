#include "lexer/lexer.h"

#include "diagnostics/consumer/null.h"
#include "lexer/token_matchers.h"
#include "nth/test/test.h"

namespace ic::lex {
namespace {

using ::ic::testing::HasIntegerValue;
using ::ic::testing::HasKind;
using ::nth::debug::ElementsAreSequentially;

NTH_TEST("lex/empty") {
  diag::NullConsumer d;
  auto token_buffer = Lex("", d);
  NTH_EXPECT(token_buffer.size() == 1);
}

NTH_TEST("lex/keyword") {
  diag::NullConsumer d;
  auto token_buffer = Lex("let", d);
  NTH_ASSERT(token_buffer.size() == 2);
  NTH_EXPECT(token_buffer[0].kind() == Token::Kind::Let);

  token_buffer = Lex("var", d);
  NTH_ASSERT(token_buffer.size() == 2);
  NTH_EXPECT(token_buffer[0].kind() == Token::Kind::Var);

  token_buffer = Lex("true", d);
  NTH_ASSERT(token_buffer.size() == 2);
  NTH_EXPECT(token_buffer[0].kind() == Token::Kind::True);

  token_buffer = Lex("false", d);
  NTH_ASSERT(token_buffer.size() == 2);
  NTH_EXPECT(token_buffer[0].kind() == Token::Kind::False);

  token_buffer = Lex("builtin", d);
  NTH_ASSERT(token_buffer.size() == 2);
  NTH_EXPECT(token_buffer[0].kind() == Token::Kind::Builtin);
}

NTH_TEST("lex/identifier", std::string_view id) {
  diag::NullConsumer d;
  auto token_buffer = Lex(id, d);
  NTH_ASSERT(token_buffer.size() == 2);
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
  NTH_EXPECT(token_buffer >>= ElementsAreSequentially(
                 HasIntegerValue(expected), HasKind(Token::Kind::Eof)));
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
                 HasKind(Token::Kind::ColonColonEqual), HasIntegerValue(3),
                 HasKind(Token::Kind::Eof)));
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

NTH_TEST("lex/comma") {
  diag::NullConsumer d;
  auto token_buffer = Lex(R"(,)", d);
  NTH_EXPECT(token_buffer >>= ElementsAreSequentially(
                 HasKind(Token::Kind::Comma), HasKind(Token::Kind::Eof)));
}

NTH_TEST("lex/period") {
  diag::NullConsumer d;
  auto token_buffer = Lex(R"(.)", d);
  NTH_EXPECT(token_buffer >>= ElementsAreSequentially(
                 HasKind(Token::Kind::Period), HasKind(Token::Kind::Eof)));
}

NTH_TEST("lex/semicolon") {
  diag::NullConsumer d;
  auto token_buffer = Lex(R"(;)", d);
  NTH_EXPECT(token_buffer >>= ElementsAreSequentially(
                 HasKind(Token::Kind::Semicolon), HasKind(Token::Kind::Eof)));
}

NTH_TEST("lex/string-literal", std::string_view content) {
  diag::NullConsumer d;
  auto token_buffer = Lex(content, d);
  NTH_EXPECT(token_buffer >>=
             ElementsAreSequentially(HasKind(Token::Kind::StringLiteral),
                                     HasKind(Token::Kind::Eof)));
}

NTH_INVOKE_TEST("lex/string-literal") {
  for (std::string_view content : {
           R"("")",
           R"("abc")",
           R"("\"")",
           R"("\n")",
           R"("\r")",
           R"("\t")",
           R"("\\")",
           R"("\\\\")",
           R"("abc\tdef")",
           R"("abc\\tdef")",
       }) {
    co_yield content;
  }
}

NTH_TEST("lex/character-literal", std::string_view content) {
  diag::NullConsumer d;
  auto token_buffer = Lex(content, d);

  NTH_EXPECT(token_buffer >>=
             ElementsAreSequentially(HasKind(Token::Kind::CharacterLiteral),
                                     HasKind(Token::Kind::Eof)));
}

NTH_INVOKE_TEST("lex/character-literal") {
  for (std::string_view content :
       {R"(!'a')", R"(!'b')", R"(!'c')", R"(!'!')", R"(!'\0')", R"(!'\t')",
        R"(!'\n')", R"(!'\r')", R"(!'\'')", R"(!'\\')", R"(!'\0')"}) {
    co_yield content;
  }
}

}  // namespace
}  // namespace ic::lex
