#include "toolchain/lexer/lexer.h"

#include "nth/test/test.h"

namespace ic {
namespace {

using ::nth::ElementsAreSequentially;

inline constexpr nth::ExpectationMatcher HasKind("has-kind",
                                                 [](auto const &value,
                                                    Token::Kind kind) {
                                                   return value.kind() == kind;
                                                 });

inline constexpr nth::ExpectationMatcher HasImmediateIntegerValue(
    "has-kind", [](auto const &value, uint32_t number) {
      return value.kind() == Token::Kind::Integer and
             value.AsIntegerPayload() ==
                 Token::IntegerPayload::Immediate(number);
    });

NTH_TEST("lex/empty") {
  DiagnosticConsumer d;
  auto token_buffer = Lex("", d);
  NTH_EXPECT(token_buffer.size() == 0);
}

NTH_TEST("lex/identifier", std::string_view id) {
  DiagnosticConsumer d;
  auto token_buffer = Lex(id, d);
  NTH_EXPECT(token_buffer.size() == 1) NTH_ELSE { return; }
  NTH_EXPECT(token_buffer[0].kind() == Token::Kind::Identifier);
}

NTH_INVOKE_TEST("lex/identifier") {
  for (std::string_view id :
       {"a", "name", "_blah", "_17", "blah__", "blah_17_", "___"}) {
    co_yield id;
  }
}


NTH_TEST("lex/integer", std::string_view n, uint32_t expected) {
  DiagnosticConsumer d;
  auto token_buffer = Lex(n, d);
  NTH_EXPECT(token_buffer >>=
             ElementsAreSequentially(HasImmediateIntegerValue(expected)));
}

NTH_INVOKE_TEST("lex/integer") {
  co_yield nth::TestArguments{"0", 0};
  co_yield nth::TestArguments{"0d0", 0};
  co_yield nth::TestArguments{"123", 123};
  co_yield nth::TestArguments{"0d123", 123};
}

NTH_TEST("lex/basic") {
  DiagnosticConsumer d;
  auto token_buffer = Lex("x ::= 3", d);
  NTH_EXPECT(token_buffer >>=
             ElementsAreSequentially(HasKind(Token::Kind::Identifier),
                                     HasKind(Token::Kind::ColonColonEqual),
                                     HasImmediateIntegerValue(3)));
}

}  // namespace
}  // namespace ic
