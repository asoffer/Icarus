#include "lexer/lex.h"

#include "lexer/lex_impl.h"

namespace ic::lex {

std::string_view ConsumeIdentifier(std::string_view &source) {
  return ConsumeWhile<IdentifierCharacter>(source);
}

}  // namespace ic::lex
