#ifndef ICARUS_LEXER_TOKEN_MATCHERS_H
#define ICARUS_LEXER_TOKEN_MATCHERS_H

#include "common/identifier.h"
#include "lexer/token.h"
#include "nth/debug/property/property.h"

namespace ic::testing {

// Matches an identifier token whose identifier is given by `index`.
inline constexpr auto IsIdentifier = nth::debug::MakeProperty<"is-identifier">(
    [](auto const &value, Identifier index) {
      return value.kind() == Token::Kind::Identifier and
             value.Identifier() == index;
    });

// Matches a token whose kind matches `kind_matcher`.
inline constexpr auto HasKind = nth::debug::MakeProperty<"has-kind">(
    [](auto const &value, auto const &kind_matcher) {
      return nth::debug::Matches(kind_matcher, value.kind());
    });

// Matches a token representing an integer holding `number` as its immediate
// value.
inline constexpr auto HasIntegerValue =
    nth::debug::MakeProperty<"has-integer-value">(
        [](auto const &value, Integer number) {
          return value.kind() == Token::Kind::IntegerLiteral and
                 value.AsInteger() == number;
        });

// Matches a token representing a boolean value holding `b`.
inline constexpr auto HasBooleanValue =
    nth::debug::MakeProperty<"has-boolean-value">(
        [](auto const &value, bool b) {
          return (value.kind() == Token::Kind::True and b) or
                 (value.kind() == Token::Kind::False and not b);
        });

}  // namespace ic::testing

#endif  // ICARUS_LEXER_TOKEN_MATCHERS_H
