#include "lexer/lexer.h"

#include <cctype>
#include <concepts>
#include <string_view>
#include <vector>

#include "lexer/lex.h"
#include "lexer/lex_impl.h"
#include "nth/debug/debug.h"

namespace ic::lex {
namespace {

struct Lexer {
  explicit Lexer(TokenBuffer& token_buffer, char const* start)
      : token_buffer_(token_buffer), start_(start) {}

  bool TryLexKeywordOrIdentifier(std::string_view& source);
  bool TryLexNumber(std::string_view& source);
  bool TryLexOperator(std::string_view& source);
  bool TryLexStringLiteral(std::string_view& source);
  bool TryLexCharacterLiteral(std::string_view& source);
  bool TryLexComment(std::string_view& source);

  uint32_t StartIndex(std::string_view s) const { return s.data() - start_; }
  uint32_t StartIndex(char const* s) const { return s - start_; }

  void PushOpen(Token::Kind k) { opens_.emplace_back(k, token_buffer_.size()); }

  uint32_t PairClose(Token::Kind k) {
    // TODO: Emit a diagnostic.
    NTH_REQUIRE((v.always), not opens_.empty());
    NTH_REQUIRE((v.always), k == ClosingPairFor(opens_.back().first));
    uint32_t n = opens_.back().second;
    opens_.pop_back();
    return n;
  }

 private:
  static constexpr Token::Kind ClosingPairFor(Token::Kind k) {
    return static_cast<Token::Kind>(
        static_cast<std::underlying_type_t<Token::Kind>>(k) + 1);
  }

  TokenBuffer& token_buffer_;
  char const* start_;
  std::vector<std::pair<Token::Kind, uint32_t>> opens_;
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

    // We need to special-case `[*]` because otherwise we'll lex `[`.
    if (source.starts_with("[*]")) {
      [[maybe_unused]] bool b = lexer.TryLexOperator(source);
      NTH_REQUIRE((v.debug), b);
      continue;
    }

    switch (source.front()) {
#define IC_XMACRO_TOKEN_KIND_OPEN(kind, symbol)                                \
  case symbol:                                                                 \
    lexer.PushOpen(Token::Kind::kind);                                         \
    buffer.Append(Token::Symbol(Token::Kind::kind, lexer.StartIndex(source))); \
    source.remove_prefix(1);                                                   \
    continue;
#define IC_XMACRO_TOKEN_KIND_CLOSE(kind, symbol)                               \
  case symbol:                                                                 \
    buffer.AppendClose(Token::Kind::kind, lexer.PairClose(Token::Kind::kind),  \
                       lexer.StartIndex(source));                              \
    source.remove_prefix(1);                                                   \
    continue;
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
    if (lexer.TryLexCharacterLiteral(source)) { continue; }

    break;
  }
  buffer.Append(Token::Eof());
  return buffer;
}

bool Lexer::TryLexCharacterLiteral(std::string_view& source) {
  // TODO: Actually most of these situations where you're returning false are
  // diagnosable errors because you know nothing else will match.
  if (source.size() < 4 or not source.starts_with("!'")) { return false; }
  switch (source[2]) {
    case '\\': {
      if (source.size() < 5) { return false; }
      char c;
      switch (source[3]) {
        case 'n': c = '\n'; break;
        case 'r': c = '\r'; break;
        case 't': c = '\t'; break;
        case '\'': c = '\''; break;
        case '\\': c = '\\'; break;
        case '0': c = '\0'; break;
        default: return false;
      }

     if (source[4] == '\'') {
        token_buffer_.Append(Token::CharacterLiteral(c, StartIndex(source)));
        source.remove_prefix(5);
        return true;
      } else {
        return false;
      }
    } break;
    case '\'': NTH_UNIMPLEMENTED(); break;
    default:
      if (source[3] == '\'') {
        token_buffer_.Append(
            Token::CharacterLiteral(source[2], StartIndex(source)));
        source.remove_prefix(4);
        return true;
      } else {
        return false;
      }
  }
}

bool Lexer::TryLexComment(std::string_view& source) {
  if (source.size() < 2 or source.substr(0, 2) != "//") { return false; }
  size_t index = source.find("\n");
  source.remove_prefix(std::min(index, source.size()));
  return true;
}

bool Lexer::TryLexKeywordOrIdentifier(std::string_view& source) {
  NTH_REQUIRE((v.harden), not source.empty());
  if (not LeadingIdentifierCharacter(source.front())) { return false; }
  std::string_view identifier = lex::ConsumeIdentifier(source);
  token_buffer_.AppendKeywordOrIdentifier(identifier, StartIndex(identifier));
  return true;
}

bool Lexer::TryLexNumber(std::string_view& source) {
  NTH_REQUIRE((v.harden), not source.empty());
  char const* start = source.data();
  if (source.front() == '0') {
    if (source.size() == 1) {
      token_buffer_.AppendIntegerLiteral(source.substr(0, 1),
                                         StartIndex(source.substr(0, 1)));
      source.remove_prefix(1);
      return true;
    } else {
      if (IdentifierCharacter(source[1])) {
        switch (source[1]) {
          case 'b': NTH_UNIMPLEMENTED();
          case 'd': {
            source.remove_prefix(2);
            goto consume_decimal;
          }
          case 'x': NTH_UNIMPLEMENTED();
          default: NTH_UNIMPLEMENTED("{}") <<= {source[1]};
        }
      } else {
        goto consume_decimal;
      }
      switch (source[1]) {
        case 'b': NTH_UNIMPLEMENTED();
        case 'd': {
          source.remove_prefix(2);
          goto consume_decimal;
        }
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
  NTH_REQUIRE((v.harden), not source.empty());
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
  NTH_REQUIRE((v.harden), not source.empty());
  if (not source.starts_with("\"")) { return false; }
  char const * p = source.data() + 1;
  bool escaped = false;
  std::string content;
  while (p != source.data() + source.size()) {
    if (escaped) {
      escaped = false;
      switch (*p) {
        case '\\': ++p; break;
        case 'n':
          ++p;
          content.push_back('\n');
          break;
        case 'r':
          ++p;
          content.push_back('\r');
          break;
        case 't':
          ++p;
          content.push_back('\t');
          break;
        case '"':
          ++p;
          content.push_back('"');
          break;
        case '0':
          ++p;
          content.push_back('\0');
          break;
        default:
          NTH_UNIMPLEMENTED("Invalid escape character \\{}. Emit an error.") <<=
              {*p};
      }
    } else {
      switch (*p) {
        case '\\':
          ++p;
          escaped = true;
          break;
        case '"':
          token_buffer_.AppendStringLiteral(std::move(content),
                                            StartIndex(source.substr(0, 1)));
          source.remove_prefix(++p - source.data());
          return true;
        case '\n':
          NTH_UNIMPLEMENTED(
              "Raw newline in string literal should emit an error.");
        default:
          // TODO: Check if this is a valid character or a literal '\r', '\t' or
          // something similar.
          content.push_back(*p++); break;
      }
    }
  }

  NTH_UNIMPLEMENTED("Reached the end of the file without closing the string.");
  return false;
}

}  // namespace ic::lex
