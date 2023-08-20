#include "toolchain/parser/parser.h"

#include "nth/test/test.h"
#include "toolchain/lexer/lexer.h"

namespace ic {

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

inline constexpr nth::ExpectationMatcher Anything("anything", [](auto const &) {
  return true;
});

inline constexpr nth::ExpectationMatcher HasSubtreeSize(
    "has-subtree-size",
    [](auto const &value, auto const &subtree_size_matcher) {
      return nth::Matches(subtree_size_matcher, value.subtree_size);
    });

inline constexpr nth::ExpectationMatcher HasToken(
    "has-token", [](auto const &value, auto const &token_matcher) {
      return nth::Matches(token_matcher, value.token);
    });

NTH_TEST("parser/empty", std::string_view content) {
  DiagnosticConsumer d;
  TokenBuffer buffer = Lex(content, d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially());
}

NTH_INVOKE_TEST("parser/empty") {
  co_yield "";
  co_yield "\n";
  co_yield "\n\n";
  co_yield "\n   \n";
}

NTH_TEST("parser/declaration") {
  DiagnosticConsumer d;
  TokenBuffer buffer = Lex("let x ::= 3", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>=
             ElementsAreSequentially(HasToken(HasKind(Token::Kind::Identifier)),
                                     HasToken(HasKind(Token::Kind::Integer)),
                                     HasSubtreeSize(3), HasSubtreeSize(4)));
}

NTH_TEST("parser/multiple-declarations-with-newlines") {
  DiagnosticConsumer d;
  TokenBuffer buffer = Lex(R"(
  let x ::= 3
  var y ::= 4
  )",
                           d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 HasToken(HasKind(Token::Kind::Identifier)),
                 HasToken(HasKind(Token::Kind::Integer)), HasSubtreeSize(3),
                 HasToken(HasKind(Token::Kind::Identifier)),
                 HasToken(HasKind(Token::Kind::Integer)), HasSubtreeSize(3),
                 HasSubtreeSize(7)));
}

}  // namespace ic
