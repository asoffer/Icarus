#include "parser/parser.h"

#include "diagnostics/consumer/null.h"
#include "lexer/lexer.h"
#include "lexer/token_matchers.h"
#include "nth/test/test.h"

namespace ic {

using ::ic::testing::HasBooleanValue;
using ::ic::testing::HasImmediateIntegerValue;
using ::ic::testing::HasKind;
using ::ic::testing::IsIdentifier;
using ::nth::ElementsAreSequentially;

inline constexpr auto HasSubtreeSize =
    nth::ExpectationMatcher<"has-subtree-size">(
        [](auto const &value, auto const &subtree_size_matcher) {
          return nth::Matches(subtree_size_matcher, value.subtree_size);
        });

inline constexpr auto HasToken = nth::ExpectationMatcher<"has-token">(
    [](auto const &value, auto const &token_matcher) {
      return nth::Matches(token_matcher, value.token);
    });

inline constexpr auto ExpressionGroup =
    nth::ExpectationMatcher<"expression-group">([](auto const &value) {
      return value.kind == ParseTree::Node::Kind::ExpressionGroup;
    });

// Matches an identifier token whose identifier is given by `id`.
auto IdentifierToken(TokenBuffer &buffer, std::string_view id) {
  return HasToken(IsIdentifier(buffer.IdentifierIndex(id)));
}

inline constexpr auto InfixOperator = nth::ExpectationMatcher<"infix-operator">(
    [](auto const &value, auto const &op_matcher) {
      return value.kind == ParseTree::Node::Kind::InfixOperator and
             nth::Matches(value.token.kind(), op_matcher);
    });

NTH_TEST("parser/empty", std::string_view content) {
  diag::NullConsumer d;
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
  diag::NullConsumer d;
  TokenBuffer buffer = Lex("let x ::= 3", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 HasToken(HasImmediateIntegerValue(3)),
                 IdentifierToken(buffer, "x") and HasSubtreeSize(2),
                 HasSubtreeSize(3)));
}

NTH_TEST("parser/declaration") {
  diag::NullConsumer d;
  TokenBuffer buffer = Lex("let x ::= true", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 HasToken(HasBooleanValue(true)),
                 IdentifierToken(buffer, "x") and HasSubtreeSize(2),
                 HasSubtreeSize(3)));
}

NTH_TEST("parser/comment") {
  diag::NullConsumer d;
  TokenBuffer buffer = Lex("let x ::= true  // comment!", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 HasToken(HasBooleanValue(true)),
                 IdentifierToken(buffer, "x") and HasSubtreeSize(2),
                 HasSubtreeSize(3)));
}

NTH_TEST("parser/multiple-declarations-with-newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = Lex(R"(
  let x ::= 3
  var y ::= 4
  )",
                           d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 HasToken(HasImmediateIntegerValue(3)),
                 IdentifierToken(buffer, "x") and HasSubtreeSize(2),
                 HasToken(HasImmediateIntegerValue(4)),
                 IdentifierToken(buffer, "y") and HasSubtreeSize(2),
                 HasSubtreeSize(5)));
}

NTH_TEST("parser/operator-precedence/plus-times") {
  diag::NullConsumer d;
  TokenBuffer buffer = Lex(R"(x + y * z)", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 IdentifierToken(buffer, "x"), InfixOperator(Token::Kind::Plus),
                 IdentifierToken(buffer, "y"), InfixOperator(Token::Kind::Star),
                 IdentifierToken(buffer, "z"),
                 ExpressionGroup() and HasSubtreeSize(4),
                 ExpressionGroup() and HasSubtreeSize(7), HasSubtreeSize(8)));
}

NTH_TEST("parser/operator-precedence/times-plus") {
  diag::NullConsumer d;
  TokenBuffer buffer = Lex(R"(x * y + z)", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 IdentifierToken(buffer, "x"), InfixOperator(Token::Kind::Star),
                 IdentifierToken(buffer, "y"),
                 ExpressionGroup() and HasSubtreeSize(4),
                 InfixOperator(Token::Kind::Plus), IdentifierToken(buffer, "z"),
                 ExpressionGroup() and HasSubtreeSize(7), HasSubtreeSize(8)));
}

NTH_TEST("parser/operator-precedence/plus-plus") {
  diag::NullConsumer d;
  TokenBuffer buffer = Lex(R"(x + y + z)", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 IdentifierToken(buffer, "x"), InfixOperator(Token::Kind::Plus),
                 IdentifierToken(buffer, "y"), InfixOperator(Token::Kind::Plus),
                 IdentifierToken(buffer, "z"),
                 ExpressionGroup() and HasSubtreeSize(6), HasSubtreeSize(7)));
}

}  // namespace ic
