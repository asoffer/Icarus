#ifndef ICARUS_LEXER_LEX_H
#define ICARUS_LEXER_LEX_H

#include <cctype>
#include <string_view>

namespace ic::lex {

std::string_view ConsumeIdentifier(std::string_view& source);

constexpr bool IdentifierCharacter(char c) {
  return std::isalnum(c) or c == '_';
}

}  // namespace ic::lex

#endif  // ICARUS_LEXER_LEX_H
