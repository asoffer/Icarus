#include "frontend/lex/lex.h"

#include <utility>

#include "frontend/lex/char_range.h"

// TODO: The lexer's error messages do not refer to the source locations at
// which the errors are encountered yet.

// TODO: Parsing for character literals, numbers, etc. Has not been done yet.

namespace frontend {
namespace {

// Lexer will not process more than 1 terabyte of data. The precise value here
// is chosen somewhat arbitrarily. We need a limit but do not expect files this
// large in practice anyway.
constexpr size_t kMaxLexableBytes = uint64_t{1024} * 1024 * 1024 * 1024;

struct MaxLexableBytesExceeded {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "max-lexable-bytes-exceeded";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Lexer can only process at most %u bytes but the "
                         "provided input has %u bytes.",
                         kMaxLexableBytes, length));
  }
  size_t length;
};

struct SingleBackslash {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "single-backslash";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Single backslash character encountered."));
  }
};

struct HashFollowedByWhitespace {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "hash-followed-by-whitespace";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Hash `#` must not be followed by whitespace."));
  }
};

struct InvalidEscapedCharacterLiteral {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName = "invalid-escaped-character-literal";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Invalid escaped character literal."));
  }
};

struct ExpectedCharacterLiteralDelimiter {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName =
      "expected-character-literal-delimiter";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expected %s character literal delimiter `'`.",
                         opening ? "opening" : "closing"));
  }

  bool opening;
};

struct UnprintableSourceCharacter {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "unprintable-source-character";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(diagnostic::Text(
        "Encountered an unprintable source-character (%02x).", character));
  }
  char character;
};

struct InvalidOperator {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "invalid-operator";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(diagnostic::Text("Invalid operator"));
  }
};

struct UnmatchedDelimiter {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "unmatched-delimiter";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Unmatched delimiter."));
  }
};

struct InvalidCharacterFollowingNumber {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName =
      "invalid-character-following-number";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Invalid character following number."));
  }
};

struct EndOfLineInStringLiteral {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "eol-in-string-literal";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(diagnostic::Text(
        "No closing `\"` for string literal before end of line."));
  }
};

struct EndOfFileInStringLiteral {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "eof-in-string-literal";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(diagnostic::Text(
        "No closing `\"` for string literal before end of file."));
  }
};

std::optional<Lexeme> ConsumeCharLiteral(
    char_range &range, diagnostic::DiagnosticConsumer &diagnostic_consumer) {
  ASSERT(range.size() >= 2);
  ASSERT(range[0] == '!');
  ASSERT(range[1] == '\'');
  char const *p = range.data() + 2;

  char c;
  if (*p == '\\') {
    ++p;
    switch (*p) {
      case '\\':
      case 'a': c = '\a'; break;
      case 'b': c = '\b'; break;
      case 'f': c = '\f'; break;
      case 'n': c = '\n'; break;
      case 'r': c = '\r'; break;
      case 't': c = '\t'; break;
      case 'v': c = '\v'; break;
      case '0': c = '\0'; break;
      default: {
        diagnostic_consumer.Consume(InvalidEscapedCharacterLiteral{});
      } break;
    }
  }

  ++p;
  if (*p != '\'') {
    diagnostic_consumer.Consume(
        ExpectedCharacterLiteralDelimiter{.opening = false});
    return std::nullopt;
  }

  size_t length = p - range.data();
  return Lexeme(Lexeme::Kind::Character, range.extract_prefix(length));
}

Lexeme ConsumeComment(char_range &range) {
  ASSERT(range.size() >= 2);
  ASSERT(range[0] == '/');
  ASSERT(range[1] == '/');
  char const *p   = range.data();
  char const *end = range.end();
  while (p < end and not IsVerticalWhitespace(*p)) { ++p; }
  return Lexeme(Lexeme::Kind::Comment, range.extract_prefix(p - range.data()));
}

std::optional<Lexeme> ConsumeOperator(
    char_range &range, diagnostic::DiagnosticConsumer &diagnostic_consumer) {
  ASSERT(range.size() > 0);
  if (range.size() >= 2 and range[0] == '-' and range[1] == '-') {
    return Lexeme(Lexeme::Kind::Identifier, range.extract_prefix(2));
  }

  constexpr std::array kOperators{
      "@",   "[*]", "$",  "+=", "+",  "-=", "..", "->", "-",  "*=", "*",
      "%=",  "%",   "&=", "&",  "|=", "|",  "^=", "^",  ">>", ">=", ">",
      "::=", ":?",  "::", ":=", ".",  "!=", ":",  "<<", "<=", "<",  "==",
      "=>",  "=",   "'",  "~",  ";",  "`",  "/=", "/",
  };

  if (range.starts_with(",")) {
    return Lexeme(Lexeme::Kind::Comma, range.extract_prefix(1));
  }

  for (std::string_view op : kOperators) {
    if (range.starts_with(op)) {
      return Lexeme(Lexeme::Kind::Operator, range.extract_prefix(op.size()));
    }
  }

  // Delimiters are processed after operators, because some operators contain
  // (balanced) delimiters and it's easier to search for them first than to
  // attempt to match them in the parser. The key example here is the unary
  // operator `[*]`.

  for (char delimiter : {'(', '{', '['}) {
    if (range[0] == delimiter) {
      return Lexeme(Lexeme::Kind::LeftDelimiter, range.extract_prefix(1));
    }
  }

  for (char delimiter : {')', '}', ']'}) {
    if (range[0] == delimiter) {
      return Lexeme(Lexeme::Kind::RightDelimiter, range.extract_prefix(1));
    }
  }

  if (range.starts_with("//")) {
    char const *p = range.data() + 2;
    while (not IsVerticalWhitespace(*p)) { ++p; }
    size_t length = p - range.data();
    return Lexeme(Lexeme::Kind::Comment, range.extract_prefix(length));
  }

  diagnostic_consumer.Consume(InvalidOperator{});
  return std::nullopt;
}

std::optional<Lexeme> ConsumeStringLiteral(
    char_range &range, diagnostic::DiagnosticConsumer &diagnostic_consumer) {
  ASSERT(range.size() != 0);
  char const *p = range.data();
  ++p;
  bool escaped = false;
  while (p != range.end() and not IsVerticalWhitespace(*p)) {
    switch (*p) {
      case '\0': break;
      case '\\': escaped = not escaped; break;
      case '"':
        if (not escaped) {
          ++p;
          goto end_loop;
        }
        [[fallthrough]];
      default: escaped = false;
    }

    if (not IsPrintable(*p)) {
      diagnostic_consumer.Consume(UnprintableSourceCharacter{.character = *p});
      return std::nullopt;
    }

    ++p;
  }

  if (p == range.end() or *p == '\0') {
    diagnostic_consumer.Consume(EndOfFileInStringLiteral{});
    return std::nullopt;
  } else if (IsVerticalWhitespace(*p)) {
    diagnostic_consumer.Consume(EndOfLineInStringLiteral{});
    return std::nullopt;
  }

end_loop:
  size_t length = p - range.data();
  return Lexeme(Lexeme::Kind::String, range.extract_prefix(length));
}

std::optional<Lexeme> ConsumeNumber(
    char_range &range, diagnostic::DiagnosticConsumer &diagnostic_consumer) {
  ASSERT(range.size() != 0);
  char const *p = range.data();
  if (range[0] == '0' and range.size() > 1 and
      (range[1] == 'x' or range[1] == 'o' or range[1] == 'd' or
       range[1] == 'b')) {
    p += 2;
    // Consume any possible sequence of characters that could show up in a
    // number whether or not they are a valid number, and check for validity
    // after the fact.
    while (p != range.end() and (IsHexDigit(*p) or *p == '_' or *p == '.')) {
      ++p;
    }
  } else {
    while (p != range.end() and (IsDigit(*p) or *p == '_' or *p == '.')) {
      ++p;
    }
  }
  size_t length = p - range.data();
  if (p != range.end() and IsAlpha(*p)) {
    diagnostic_consumer.Consume(InvalidCharacterFollowingNumber{});
    return std::nullopt;
  }

  return Lexeme(Lexeme::Kind::Number, range.extract_prefix(length));
}

std::optional<Lexeme> ConsumeIdentifier(char_range &range) {
  char const *p = range.data();
  while (p != range.end() and IsAlphaNumericOrUnderscore(*p)) { ++p; }
  size_t length = p - range.data();
  return Lexeme(Lexeme::Kind::Identifier, range.extract_prefix(length));
}

std::optional<Lexeme> ConsumeOneLexeme(
    char_range &range, diagnostic::DiagnosticConsumer &diagnostic_consumer) {
  ASSERT(range.size() != 0);

  // Remove all non-newline whitespace
  while (IsHorizontalWhitespace(range[0])) {
    range.remove_prefix(1);
    if (range.empty()) {
      return Lexeme(Lexeme::Kind::EndOfFile, std::string_view(range.data(), 0));
    }
  }

  // Non-emptiness invariant should still hold.
  ASSERT(range.size() != 0);

  char c = range[0];
  if (IsDigit(c)) {
    return ConsumeNumber(range, diagnostic_consumer);
  } else if (IsAlphaOrUnderscore(c)) {
    return ConsumeIdentifier(range);
  } else if (IsVerticalWhitespace(c)) {
    return Lexeme(Lexeme::Kind::Newline, range.extract_prefix(1));
  } else if (not IsPrintable(c)) {
    range.remove_prefix(1);
    diagnostic_consumer.Consume(UnprintableSourceCharacter{.character = c});
    return std::nullopt;
  }
  switch (c) {
    case '"': return ConsumeStringLiteral(range, diagnostic_consumer);
    case '\\':
      if (range.size() > 1 and range[1] == '\\') {
        return Lexeme(Lexeme::Kind::Newline, range.extract_prefix(2));
      } else {
        diagnostic_consumer.Consume(SingleBackslash{});
        return std::nullopt;
      }
      if (range.size() > 1 and IsWhitespace(range[1])) {
        diagnostic_consumer.Consume(HashFollowedByWhitespace{});
        return std::nullopt;
      }
      return Lexeme(Lexeme::Kind::Hash, range.extract_prefix(1));
    case '/': {
      if (range.size() > 1 and range[1] == '/') {
        return ConsumeComment(range);
      } else {
        goto consume_operator;
      }
    }
    case '#': {
      if (range.size() > 1 and IsWhitespace(range[1])) {
        diagnostic_consumer.Consume(HashFollowedByWhitespace{});
        return std::nullopt;
      }
      return Lexeme(Lexeme::Kind::Hash, range.extract_prefix(1));
    }
    case '!':
      if (range.size() >= 2 and range[1] == '\'') {
        return ConsumeCharLiteral(range, diagnostic_consumer);
      } else {
        goto consume_operator;
      }
    default:
    consume_operator:
      return ConsumeOperator(range, diagnostic_consumer);
  }
}

}  // namespace

struct LexResultBuilder {
  explicit LexResultBuilder(LexResult *result,
                            diagnostic::DiagnosticConsumer *diagnostic_consumer)
      : lex_result_(*ASSERT_NOT_NULL(result)),
        diagnostic_consumer_(*ASSERT_NOT_NULL(diagnostic_consumer)) {}

  bool append(Lexeme l) {
    switch (l.kind()) {
      case Lexeme::Kind::LeftDelimiter: {
        unmatched_delimiters_.push_back(lex_result_.lexemes_.size());
      } break;
      case Lexeme::Kind::RightDelimiter: {
        if (unmatched_delimiters_.empty()) {
          diagnostic_consumer_.Consume(UnmatchedDelimiter{});
          return false;
        }
        size_t match_index = unmatched_delimiters_.back();
        size_t offset      = lex_result_.lexemes_.size() - match_index;
        unmatched_delimiters_.pop_back();
        auto &matching_lexeme = lex_result_.lexemes_[match_index];
        // In ASCII, the character values for matching delimiters are always
        // within two of each other, with the right-delimiter having a higher
        // value than the left-delimiter.
        int diff = static_cast<int>(l.content()[0]) -
                   static_cast<int>(matching_lexeme.content()[0]);
        if (diff < 0 or diff > 2) {
          diagnostic_consumer_.Consume(UnmatchedDelimiter{});
          return false;
        }
        matching_lexeme.set_match_offset(offset);
        l.set_match_offset(offset);
      } break;
      default: break;
    }
    lex_result_.lexemes_.push_back(l);
    return true;
  }

  bool balanced() const { return unmatched_delimiters_.empty(); }

 private:
  std::vector<size_t> unmatched_delimiters_;
  LexResult &lex_result_;
  diagnostic::DiagnosticConsumer &diagnostic_consumer_;
};

std::optional<LexResult> Lex(
    std::string_view source,
    diagnostic::DiagnosticConsumer &diagnostic_consumer) {
  if (source.size() > kMaxLexableBytes) {
    diagnostic_consumer.Consume(MaxLexableBytesExceeded{
        .length = source.size(),
    });
    return std::nullopt;
  }

  LexResult result;
  LexResultBuilder builder(&result, &diagnostic_consumer);

  char_range range = source;
  while (not range.empty()) {
    auto maybe_lexeme = ConsumeOneLexeme(range, diagnostic_consumer);
    if (not maybe_lexeme) { return std::nullopt; }
    if (maybe_lexeme->kind() == Lexeme::Kind::Comment) { continue; }
    if (not builder.append(*std::move(maybe_lexeme))) { return std::nullopt; }
  }

  if (not builder.balanced()) { return std::nullopt; }
  return result;
}

}  // namespace frontend
