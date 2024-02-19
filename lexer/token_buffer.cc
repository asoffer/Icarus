#include "lexer/token_buffer.h"

#include "common/resources.h"
#include "nth/debug/log/log.h"
#include "nth/numeric/integer.h"

namespace ic {

void TokenBuffer::AppendIntegerLiteral(std::string_view integer,
                                       uint32_t offset) {
  nth::integer value = 0;
  for (char c : integer) {
    if (c == '_') { continue; }
    value = value * 10 + (c - '0');
  }

  tokens_.push_back(Token::IntegerLiteral(offset, std::move(value)));
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

void TokenBuffer::AppendClose(Token::Kind kind, uint32_t open_index,
                              uint32_t offset) {
  tokens_[open_index].set_payload(tokens_.size());
  tokens_.push_back(Token::CloseSymbol(kind, open_index, offset));
}

}  // namespace ic
