#include "lexer/token_buffer.h"

#include "common/resources.h"
#include "nth/debug/log/log.h"

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
    uint32_t index = static_cast<uint32_t>(
        resources.integers.index(resources.integers.insert(value).first));
    payload = Token::IntegerPayload::Index(index);
  } else {
    payload = Token::IntegerPayload::Immediate(value);
  }
  tokens_.push_back(Token::IntegerLiteral(offset, payload));
}

void TokenBuffer::AppendStringLiteral(std::string s, uint32_t offset) {
  uint32_t index = resources.StringLiteralIndex(std::move(s));
  tokens_.push_back(Token::StringLiteral(offset, index));
}

void TokenBuffer::AppendKeywordOrIdentifier(std::string_view identifier,
                                            uint32_t offset) {
#define IC_XMACRO_TOKEN_KIND_KEYWORD(kind, keyword)                            \
  if (identifier == keyword) {                                                 \
    tokens_.push_back(Token::Keyword##kind(offset));                           \
    return;                                                                    \
  }
#include "lexer/token_kind.xmacro.h"

  tokens_.push_back(Token::Identifier(offset, Identifier(identifier)));
}

}  // namespace ic
