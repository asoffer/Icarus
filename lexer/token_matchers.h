#ifndef ICARUS_LEXER_TOKEN_MATCHERS_H
#define ICARUS_LEXER_TOKEN_MATCHERS_H

#include "nth/test/test.h"
#include "lexer/token.h"

namespace ic::testing {

// Matches an identifier token whose identifier is given by `index`.
inline constexpr nth::ExpectationMatcher IsIdentifier(
    "is-identifier", [](auto const &value, uint32_t index) {
      return value.kind() == Token::Kind::Identifier and
             value.IdentifierIndex() == index;
    });

// Matches a token whose kind matches `kind_matcher`.
inline constexpr nth::ExpectationMatcher HasKind("has-kind",
                                                 [](auto const &value,
                                                    auto const &kind_matcher) {
                                                   return nth::Matches(kind_matcher, value.kind());
                                                 });

// Matches a token representing an integer holding `number` as its immediate
// value.
inline constexpr nth::ExpectationMatcher HasImmediateIntegerValue(
    "has-kind", [](auto const &value, uint32_t number) {
      return value.kind() == Token::Kind::Integer and
             value.AsIntegerPayload() ==
                 Token::IntegerPayload::Immediate(number);
    });

// Matches a token representing a boolean value holding `b`.
inline constexpr nth::ExpectationMatcher HasBooleanValue(
    "has-kind", [](auto const &value, bool b) {
      return (value.kind() == Token::Kind::True and b) or
             (value.kind() == Token::Kind::False and not b);
    });

}  // namespace ic::testing

#endif  // ICARUS_LEXER_TOKEN_MATCHERS_H
