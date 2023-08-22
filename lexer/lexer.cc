#include "lexer/lexer.h"

#include <cctype>
#include <concepts>
#include <string_view>
#include <vector>

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

  uint32_t StartIndex(std::string_view s) const { return s.data() - start_; }
  uint32_t StartIndex(char const* s) const { return s - start_; }

 private:
  TokenBuffer& token_buffer_;
  char const* start_;
};

template <auto F>
requires(std::is_invocable_r_v<bool, decltype(F), char>)  //
    std::string_view ConsumeWhile(std::string_view& source) {
  char const* end   = source.data() + source.size();
  char const* start = source.data();
  char const* p     = start;
  for (; p != end and F(*p); ++p) {}
  size_t count            = std::distance(start, p);
  std::string_view result = source.substr(0, count);
  source.remove_prefix(count);
  return result;
}

constexpr bool LeadingIdentifierCharacter(char c) {
  return std::isalpha(c) or c == '_';
}

constexpr bool IdentifierCharacter(char c) {
  return std::isalnum(c) or c == '_';
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

    ConsumeWhile<WhitespaceCharacter>(source);
    if (source.empty()) { return buffer; }

    if (lexer.TryLexKeywordOrIdentifier(source)) { continue; }
    if (lexer.TryLexNumber(source)) { continue; }
    if (lexer.TryLexOperator(source)) { continue; }
    if (lexer.TryLexStringLiteral(source)) { continue; }
    return buffer;
  }
}

bool Lexer::TryLexKeywordOrIdentifier(std::string_view& source) {
  NTH_ASSERT((v.debug), not source.empty());
  if (not LeadingIdentifierCharacter(source.front())) { return false; }
  std::string_view identifier = ConsumeWhile<IdentifierCharacter>(source);
  token_buffer_.AppendKeywordOrIdentifier(identifier, StartIndex(identifier));
  return true;
}

bool Lexer::TryLexNumber(std::string_view& source) {
  NTH_ASSERT((v.debug), not source.empty());
  char const * start = source.data();
  if (source.front() == '0') {
    if (source.size() == 1) {
      token_buffer_.AppendInteger(source.substr(0, 1),
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
    std::string_view number = ConsumeWhile<DecimalCharacter>(source);
    if (number.empty()) { return false; }
    token_buffer_.AppendInteger(number, StartIndex(start));
    return true;
  }
  NTH_UNREACHABLE();
}

bool Lexer::TryLexOperator(std::string_view& source) {
  NTH_ASSERT((v.debug), not source.empty());
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
  NTH_ASSERT((v.debug), not source.empty());
  return false;
}

}  // namespace ic
