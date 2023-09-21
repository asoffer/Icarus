#include "lexer/token.h"

#include <cstdint>

#include "nth/debug/debug.h"

namespace ic {

Token::IntegerPayload Token::IntegerPayload::Index(uint32_t index) {
  NTH_REQUIRE((v.debug), index < IntegerPayload::PayloadLimit);
  Token::IntegerPayload p;
  p.value_ = (IntegerPayload::PayloadLimit | index);
  return p;
}

Token::IntegerPayload Token::IntegerPayload::Immediate(uint32_t value) {
  NTH_REQUIRE((v.debug), value < IntegerPayload::PayloadLimit);
  Token::IntegerPayload p;
  p.value_ = value;
  return p;
}

Token Token::IntegerLiteral(uint32_t offset, IntegerPayload payload) {
  Token token;
  token.offset_  = offset;
  token.kind_    = static_cast<uint8_t>(Kind::IntegerLiteral);
  token.payload_ = payload.value();
  return token;
}

Token::IntegerPayload Token::AsIntegerPayload() const {
  NTH_REQUIRE((v.debug), kind() == Kind::IntegerLiteral);
  return IntegerPayload(payload_);
}

bool Token::AsBoolean() const {
  switch (kind()) {
    case Kind::True: return true;
    case Kind::False: return false;
    default: NTH_UNREACHABLE("{}") <<= {*this};
  }
}

Token Token::Identifier(uint32_t offset, uint32_t identifier_index) {
  NTH_REQUIRE((v.debug), identifier_index < PayloadLimit);

  Token token;
  token.offset_  = offset;
  token.kind_    = static_cast<uint8_t>(Kind::Identifier);
  token.payload_ = identifier_index;
  return token;
}

Token Token::Symbol(Token::Kind kind, uint32_t offset) {
  Token token;
  token.offset_ = offset;
  token.kind_   = static_cast<uint8_t>(kind);
  return token;
}

Token Token::Eof() {
  Token token;
  token.offset_ = -1;
  token.kind_   = static_cast<uint8_t>(Kind::Eof);
  return token;
}

Token Token::Invalid() {
  Token token;
  token.offset_ = -1;
  token.kind_   = static_cast<uint8_t>(Kind::Invalid);
  return token;
}

#define IC_XMACRO_TOKEN_KIND_KEYWORD(kind, keyword)                            \
  Token Token::Keyword##kind(uint32_t offset) {                                \
    Token token;                                                               \
    token.offset_ = offset;                                                    \
    token.kind_   = static_cast<uint8_t>(Token::Kind::kind);                   \
    return token;                                                              \
  }
#include "lexer/token_kind.xmacro.h"

uint32_t Token::IdentifierIndex() const {
  NTH_REQUIRE((v.debug), kind() == Kind::Identifier);
  return payload_;
}

}  // namespace ic
