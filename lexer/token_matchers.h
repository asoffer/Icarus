#ifndef ICARUS_LEXER_TOKEN_MATCHERS_H
#define ICARUS_LEXER_TOKEN_MATCHERS_H

#include "nth/test/test.h"
#include "lexer/token.h"

namespace ic::testing {

// Matches an identifier token whose identifier is given by `index`.
inline constexpr auto IsIdentifier = nth::ExpectationMatcher<"is-identifier">(
    [](auto const &value, uint32_t index) {
      return value.kind() == Token::Kind::Identifier and
             value.IdentifierIndex() == index;
    });

// Matches a token whose kind matches `kind_matcher`.
inline constexpr auto HasKind = nth::ExpectationMatcher<"has-kind">(
    [](auto const &value, auto const &kind_matcher) {
      return nth::Matches(kind_matcher, value.kind());
    });

// Matches a token representing an integer holding `number` as its immediate
// value.
inline constexpr auto HasImmediateIntegerValue =
    nth::ExpectationMatcher<"has-immediate-integer-value">(
        [](auto const &value, uint32_t number) {
          return value.kind() == Token::Kind::IntegerLiteral and
                 value.AsIntegerPayload() ==
                     Token::IntegerPayload::Immediate(number);
        });

// Matches a token representing a boolean value holding `b`.
inline constexpr auto HasBooleanValue =
    nth::ExpectationMatcher<"has-boolean-value">([](auto const &value, bool b) {
      return (value.kind() == Token::Kind::True and b) or
             (value.kind() == Token::Kind::False and not b);
    });

}  // namespace ic::testing

#endif  // ICARUS_LEXER_TOKEN_MATCHERS_H
