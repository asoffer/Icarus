#include "lexer/lexer.h"

#include <cctype>
#include <concepts>
#include <string_view>
#include <vector>

#include "lexer/lex.h"
#include "lexer/lex_impl.h"
#include "nth/debug/debug.h"

namespace ic {
namespace {

struct Lexer {
  explicit Lexer(TokenBuffer& token_buffer, char const* start)
      : token_buffer_(token_buffer), start_(start) {}

  bool TryLexKeywordOrIdentifier(std::string_view& source);
  bool TryLexNumber(std::string_view& source);
  bool TryLexOperator(std::string_view& source);
  bool TryLexStringLiteral(std::string_view& source);
  bool TryLexComment(std::string_view& source);

  uint32_t StartIndex(std::string_view s) const { return s.data() - start_; }
  uint32_t StartIndex(char const* s) const { return s - start_; }

 private:
  TokenBuffer& token_buffer_;
  char const* start_;
};

constexpr bool LeadingIdentifierCharacter(char c) {
  return std::isalpha(c) or c == '_';
}

constexpr bool DecimalCharacter(char c) { return std::isdigit(c) or c == '_'; }

constexpr bool WhitespaceCharacter(char c) { return std::isspace(c); }

}  // namespace

TokenBuffer Lex(std::string_view source,
                diag::DiagnosticConsumer& diagnostic_consumer) {
  TokenBuffer buffer;
  Lexer lexer(buffer, source.data());
  while (true) {
    if (not source.empty() and source.front() == '\n') {
      buffer.Append(
          Token::Symbol(Token::Kind::Newline, lexer.StartIndex(source)));
      source.remove_prefix(1);
      continue;
    }

    lex::ConsumeWhile<WhitespaceCharacter>(source);
    if (source.empty()) { break; }

    switch (source.front()) {
#define IC_XMACRO_TOKEN_KIND_ONE_CHARACTER_TOKEN(kind, symbol)                 \
  case symbol:                                                                 \
    buffer.Append(Token::Symbol(Token::Kind::kind, lexer.StartIndex(source))); \
    source.remove_prefix(1);                                                   \
    continue;
#include "lexer/token_kind.xmacro.h"
      default: break;
    }

    if (lexer.TryLexKeywordOrIdentifier(source)) { continue; }
    if (lexer.TryLexNumber(source)) { continue; }
    if (lexer.TryLexComment(source)) { continue; }
    if (lexer.TryLexOperator(source)) { continue; }
    if (lexer.TryLexStringLiteral(source)) { continue; }

    break;
  }
  buffer.Append(Token::Eof());
  return buffer;
}

bool Lexer::TryLexComment(std::string_view& source) {
  if (source.size() < 2 or source.substr(0, 2) != "//") { return false; }
  size_t index = source.find("\n");
  source.remove_prefix(std::min(index, source.size()));
  return true;
}

bool Lexer::TryLexKeywordOrIdentifier(std::string_view& source) {
  NTH_REQUIRE((v.debug), not source.empty());
  if (not LeadingIdentifierCharacter(source.front())) { return false; }
  std::string_view identifier = lex::ConsumeIdentifier(source);
  token_buffer_.AppendKeywordOrIdentifier(identifier, StartIndex(identifier));
  return true;
}

bool Lexer::TryLexNumber(std::string_view& source) {
  NTH_REQUIRE((v.debug), not source.empty());
  char const* start = source.data();
  if (source.front() == '0') {
    if (source.size() == 1) {
      token_buffer_.AppendIntegerLiteral(source.substr(0, 1),
                                         StartIndex(source.substr(0, 1)));
      source.remove_prefix(1);
      return true;
    } else {
      switch (source[1]) {
        case 'b': NTH_UNIMPLEMENTED();
        case 'd': {
          source.remove_prefix(2);
          goto consume_decimal;
        }
        case 'x': NTH_UNIMPLEMENTED();
        default: NTH_UNIMPLEMENTED();
      }
      NTH_UNREACHABLE();
    }
  } else {
  consume_decimal:
    std::string_view number = lex::ConsumeWhile<DecimalCharacter>(source);
    if (number.empty()) { return false; }
    token_buffer_.AppendIntegerLiteral(number, StartIndex(start));
    return true;
  }
  NTH_UNREACHABLE();
}

bool Lexer::TryLexOperator(std::string_view& source) {
  NTH_REQUIRE((v.debug), not source.empty());
#define IC_XMACRO_TOKEN_KIND_OPERATOR(kind, symbol)                            \
  if (source.starts_with(symbol)) {                                            \
    token_buffer_.Append(                                                      \
        Token::Symbol(Token::Kind::kind, StartIndex(source)));                 \
    source.remove_prefix(std::string_view(symbol).size());                     \
    return true;                                                               \
  }
#include "lexer/token_kind.xmacro.h"

  return false;
}

bool Lexer::TryLexStringLiteral(std::string_view& source) {
  NTH_REQUIRE((v.debug), not source.empty());
  return false;
}

}  // namespace ic
