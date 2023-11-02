#ifndef ICARUS_LEXER_TOKEN_H
#define ICARUS_LEXER_TOKEN_H

#include <array>
#include <cstdint>

#include "common/identifier.h"
#include "common/integer.h"
#include "nth/io/printer.h"
#include "nth/strings/format/universal.h"
#include "nth/strings/interpolate.h"

namespace ic {

// Represents an atomic syntactic unit of source. Tokens should be passed by
// value and generally hold minimal information directly. They include an offset
// of their starting location within an external source, a `Token::Kind` and
// possibly extra `Token::Kind` specific payloads.
struct Token {
 private:
  static constexpr uint32_t PayloadBits = 24;

 public:
  static constexpr uint32_t PayloadLimit = uint32_t{1} << PayloadBits;

  // A categorization describing the token.
  enum class Kind : uint8_t {
#define IC_XMACRO_TOKEN_KIND(kind) kind,
#include "lexer/token_kind.xmacro.h"
  };

  constexpr Kind kind() const { return static_cast<Kind>(kind_); }

  constexpr uint32_t offset() const { return offset_; }

  constexpr void set_payload(uint32_t payload) { payload_ = payload; }
  constexpr uint32_t payload() const { return payload_; }

  // Constructs an integer token at the given offset.
  static Token IntegerLiteral(uint32_t offset, Integer n);

  // Constructs a string-literal token at the given offset.
  static Token StringLiteral(uint32_t offset, uint32_t index);

  // Constructs an identifier token at the given offset.
  static Token Identifier(uint32_t offset, Identifier id);

  // Constructs a symbol token with the given kind at the given `offset`.
  static Token Symbol(Kind k, uint32_t offset);

  // Constructs a closing-symbol (e.g `]` or `}`) at the given `offset` for a
  // corresponding open-symbol whose token index is given by `open_index`
  static Token CloseSymbol(Token::Kind token, uint32_t open_index,
                           uint32_t offset);

  // Constructs a token representing the end of the lex-stream.
  static Token Eof();

  // Constructs an invalid token that may not appear in a correct lex-stream.
  static Token Invalid();

  ic::Identifier Identifier() const;

#define IC_XMACRO_TOKEN_KIND_KEYWORD(kind, keyword)                            \
  static Token Keyword##kind(uint32_t offset);
#include "lexer/token_kind.xmacro.h"

  Integer AsInteger() const;
  bool AsBoolean() const;
  uint32_t AsStringLiteralIndex() const;

  friend void NthPrint(nth::Printer auto& p, nth::FormatterFor<Token> auto& f,
                       Token t) {
    nth::Interpolate<"[{} @{}">(p, f, t.kind(), t.offset_);

    switch (t.kind()) {
      case Token::Kind::IntegerLiteral:
        nth::Interpolate<" {}">(p, f, Integer::FromRepresentation(t.payload_));
        break;
      case Token::Kind::StringLiteral:
      case Token::Kind::Identifier:
        nth::Interpolate<" #{}">(p, f, t.payload_);
        break;
      default: break;
    }
    p.write("]");
  }

  friend bool operator==(Token const&, Token const&) = default;
  friend bool operator!=(Token const&, Token const&) = default;

  template <typename H>
  friend H AbslHashValue(H h, Token t) {
    return H::combine(std::move(h), t.offset_, t.kind_, t.payload_);
  }

 private:
  uint32_t offset_;
  uint32_t kind_ : 8;
  uint32_t payload_ : PayloadBits;
};
static_assert(sizeof(Token) == 8);

void NthPrint(nth::Printer auto& p, nth::FormatterFor<Token::Kind> auto&,
              Token::Kind k) {
  static constexpr std::array KindStrings{
#define IC_XMACRO_TOKEN_KIND_OPERATOR(kind, symbol)                            \
  std::string_view("tk.(" symbol ")"),

#define IC_XMACRO_TOKEN_KIND(kind) std::string_view("tk." #kind),
#include "lexer/token_kind.xmacro.h"
  };
  p.write(KindStrings[static_cast<uint8_t>(k)]);
}

}  // namespace ic

#endif  // ICARUS_LEXER_TOKEN_H
