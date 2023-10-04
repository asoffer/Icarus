#include "parser/parser.h"

#include "diagnostics/consumer/null.h"
#include "common/resources.h"
#include "lexer/lexer.h"
#include "lexer/token_matchers.h"
#include "nth/test/test.h"

namespace ic {

using ::ic::testing::HasBooleanValue;
using ::ic::testing::HasImmediateIntegerValue;
using ::ic::testing::HasKind;
using ::ic::testing::IsIdentifier;
using ::nth::debug::ElementsAreSequentially;

inline constexpr auto HasSubtreeSize =
    nth::debug::MakeProperty<"has-subtree-size">(
        [](auto const &value, auto const &subtree_size_matcher) {
          return nth::debug::Matches(subtree_size_matcher, value.subtree_size);
        });

inline constexpr auto HasToken = nth::debug::MakeProperty<"has-token">(
    [](auto const &value, auto const &token_matcher) {
      return nth::debug::Matches(token_matcher, value.token);
    });

inline constexpr auto ExpressionPrecedenceGroup =
    nth::debug::MakeProperty<"expression-precedence-group">(
        [](auto const &value) {
          return value.kind == ParseTree::Node::Kind::ExpressionPrecedenceGroup;
        });

inline constexpr auto MemberExpression =
    nth::debug::MakeProperty<"member-expression">([](auto const &value) {
      return value.kind == ParseTree::Node::Kind::MemberExpression;
    });

inline constexpr auto CallExpression =
    nth::debug::MakeProperty<"call-expression">([](auto const &value) {
      return value.kind == ParseTree::Node::Kind::CallExpression;
    });

inline constexpr auto Let = nth::debug::MakeProperty<"`let`">(
    [](auto const &value) { return value.kind == ParseTree::Node::Kind::Let; });

inline constexpr auto Var = nth::debug::MakeProperty<"`var`">(
    [](auto const &value) { return value.kind == ParseTree::Node::Kind::Var; });

inline constexpr auto DeclaredIdentifier =
    nth::debug::MakeProperty<"declared-identifier">([](auto const &value) {
      return value.kind == ParseTree::Node::Kind::DeclaredIdentifier;
    });

inline constexpr auto ColonEqual =
    nth::debug::MakeProperty<"`:=`">([](auto const &value) {
      return value.kind == ParseTree::Node::Kind::ColonEqual;
    });

inline constexpr auto ColonColonEqual =
    nth::debug::MakeProperty<"`::=`">([](auto const &value) {
      return value.kind == ParseTree::Node::Kind::ColonColonEqual;
    });

// debug::Matches an identifier token whose identifier is given by `id`.
auto IdentifierToken(std::string_view id) {
  return HasToken(IsIdentifier(resources.IdentifierIndex(id)));
}

inline constexpr auto InfixOperator =
    nth::debug::MakeProperty<"infix-operator">(
        [](auto const &value, auto const &op_matcher) {
          return value.kind == ParseTree::Node::Kind::InfixOperator and
                 nth::debug::Matches(op_matcher, value.token.kind());
        });

NTH_TEST("parser/empty", std::string_view content) {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(content, d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially());
}

NTH_INVOKE_TEST("parser/empty") {
  co_yield "";
  co_yield "\n";
  co_yield "\n\n";
  co_yield "\n   \n";
}

NTH_TEST("parser/declaration/integer") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x ::= 3", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 Let(), DeclaredIdentifier() and IdentifierToken("x"),
                 ColonColonEqual(), HasToken(HasImmediateIntegerValue(3)),
                 HasSubtreeSize(5), HasSubtreeSize(6)));
}

NTH_TEST("parser/declaration/bool") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x := true", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 Let(), DeclaredIdentifier() and IdentifierToken("x"),
                 ColonEqual(), HasToken(HasBooleanValue(true)),
                 HasSubtreeSize(5), HasSubtreeSize(6)));
}

NTH_TEST("parser/comment") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex("let x ::= true  // comment!", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 Let(), DeclaredIdentifier() and IdentifierToken("x"),
                 ColonColonEqual(), HasToken(HasBooleanValue(true)),
                 HasSubtreeSize(5), HasSubtreeSize(6)));
}

NTH_TEST("parser/multiple-declarations-with-newlines") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(
  let x ::= 3
  var y ::= 4
  )",
                                d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 Let(), DeclaredIdentifier() and IdentifierToken("x"),
                 ColonColonEqual(), HasToken(HasImmediateIntegerValue(3)),
                 HasSubtreeSize(5), Var(),
                 DeclaredIdentifier() and IdentifierToken("y"),
                 ColonColonEqual(), HasToken(HasImmediateIntegerValue(4)),
                 HasSubtreeSize(5), HasSubtreeSize(11)));
}

NTH_TEST("parser/operator-precedence/plus-times") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(x + y * z)", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 IdentifierToken("x"), InfixOperator(Token::Kind::Plus),
                 IdentifierToken("y"), InfixOperator(Token::Kind::Star),
                 IdentifierToken("z"),
                 ExpressionPrecedenceGroup() and HasSubtreeSize(4),
                 ExpressionPrecedenceGroup() and HasSubtreeSize(7),
                 HasSubtreeSize(8)));
}

NTH_TEST("parser/operator-precedence/times-plus") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(x * y + z)", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 IdentifierToken("x"), InfixOperator(Token::Kind::Star),
                 IdentifierToken("y"),
                 ExpressionPrecedenceGroup() and HasSubtreeSize(4),
                 InfixOperator(Token::Kind::Plus), IdentifierToken("z"),
                 ExpressionPrecedenceGroup() and HasSubtreeSize(7),
                 HasSubtreeSize(8)));
}

NTH_TEST("parser/operator-precedence/plus-plus") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(x + y + z)", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 IdentifierToken("x"), InfixOperator(Token::Kind::Plus),
                 IdentifierToken("y"), InfixOperator(Token::Kind::Plus),
                 IdentifierToken("z"),
                 ExpressionPrecedenceGroup() and HasSubtreeSize(6),
                 HasSubtreeSize(7)));
}

NTH_TEST("parser/access/basic") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b)", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(
      tree.nodes() >>= ElementsAreSequentially(
          IdentifierToken("a"),
          MemberExpression() and IdentifierToken("b") and HasSubtreeSize(2),
          HasSubtreeSize(3)));
}

NTH_TEST("parser/access/nested") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b.c)", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(
      tree.nodes() >>= ElementsAreSequentially(
          IdentifierToken("a"),
          MemberExpression() and IdentifierToken("b") and HasSubtreeSize(2),
          MemberExpression() and IdentifierToken("c") and HasSubtreeSize(3),
          HasSubtreeSize(4)));
}

NTH_TEST("parser/access/precedence") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b * c.d)", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(
      tree.nodes() >>= ElementsAreSequentially(
          IdentifierToken("a"),
          MemberExpression() and IdentifierToken("b") and HasSubtreeSize(2),
          InfixOperator(Token::Kind::Star), IdentifierToken("c"),
          MemberExpression() and IdentifierToken("d") and HasSubtreeSize(2),
          ExpressionPrecedenceGroup() and HasSubtreeSize(6),
          HasSubtreeSize(7)));
}

NTH_TEST("parser/invoke/empty") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(f())", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(tree.nodes() >>= ElementsAreSequentially(
                 IdentifierToken("f"), CallExpression() and HasSubtreeSize(2),
                 HasSubtreeSize(3)));
}

NTH_TEST("parser/invoke/empty-member-call") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b())", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(
      tree.nodes() >>= ElementsAreSequentially(
          IdentifierToken("a"),
          MemberExpression() and IdentifierToken("b") and HasSubtreeSize(2),
          CallExpression() and HasSubtreeSize(3), HasSubtreeSize(4)));
}

NTH_TEST("parser/invoke/double-call") {
  diag::NullConsumer d;
  TokenBuffer buffer = lex::Lex(R"(a.b()())", d);
  auto tree          = Parse(buffer, d);
  NTH_EXPECT(
      tree.nodes() >>= ElementsAreSequentially(
          IdentifierToken("a"),
          IdentifierToken("b") and MemberExpression() and HasSubtreeSize(2),
          CallExpression() and HasSubtreeSize(3),
          CallExpression() and HasSubtreeSize(4), HasSubtreeSize(5)));
}

}  // namespace ic
