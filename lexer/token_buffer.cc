#include "lexer/token_buffer.h"

namespace ic {

void TokenBuffer::AppendIntegerLiteral(std::string_view integer,
                                       uint32_t offset) {
  uint64_t value = 0;
  // TODO: Overflow detection.
  for (char c : integer) {
    if (c == '_') { continue; }
    value = value * 10 + (c - '0');
  }

  Token::IntegerPayload payload;
  if (value >= Token::IntegerPayload::PayloadLimit) {
    uint32_t index =
        static_cast<uint32_t>(integers_.index(integers_.insert(value).first));
    payload = Token::IntegerPayload::Index(index);
  } else {
    payload = Token::IntegerPayload::Immediate(value);
  }
  tokens_.push_back(Token::IntegerLiteral(offset, payload));
}

uint32_t TokenBuffer::IdentifierIndex(std::string_view identifier) {
  return static_cast<uint32_t>(
      identifiers_.index(identifiers_.insert(identifier).first));
}

void TokenBuffer::AppendKeywordOrIdentifier(std::string_view identifier,
                                            uint32_t offset) {
#define IC_XMACRO_TOKEN_KIND_KEYWORD(kind, keyword)                            \
  if (identifier == keyword) {                                                 \
    tokens_.push_back(Token::Keyword##kind(offset));                           \
    return;                                                                    \
  }
#include "lexer/token_kind.xmacro.h"

  tokens_.push_back(Token::Identifier(offset, IdentifierIndex(identifier)));
}

}  // namespace ic
