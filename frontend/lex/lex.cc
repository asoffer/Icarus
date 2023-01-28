#include "frontend/lex/lex.h"

#include <cctype>
#include <cmath>

#include "nth/meta/sequence.h"
#include "nth/meta/type.h"
#include "absl/container/flat_hash_map.h"
#include "absl/container/node_hash_set.h"
#include "absl/status/statusor.h"
#include "ast/ast.h"
#include "core/type_system/type.h"
#include "frontend/lex/numbers.h"
#include "frontend/lex/operators.h"
#include "frontend/lex/syntax.h"
#include "nth/numeric/integer.h"
#include "semantic_analysis/type_system.h"

namespace frontend {
namespace {

std::string_view ConsumeWhile(std::string_view &s,
                              std::predicate<char> auto &&pred) {
  char const *start = s.data();
  char const *p     = s.data();
  while (pred(*p)) { ++p; }
  size_t length = p - s.data();
  s.remove_prefix(length);
  return std::string_view(start, p);
}

absl::node_hash_set<std::string> GlobalStringTable;

struct NumberParsingFailure {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "number-parsing-failure";

  diagnostic::DiagnosticMessage ToMessage() const {
    std::string message;
    switch (error) {
      case NumberParsingError::kUnknownBase:
        message = "Unknown base for numeric literal.";
        break;
      case NumberParsingError::kTooManyDots:
        message = "Too many `.` characters in numeric literal.";
        break;
      case NumberParsingError::kNoDigits:
        message = "No digits in numeric literal.";
        break;
      case NumberParsingError::kInvalidDigit:
        message = "Invalid digit encountered in numeric literal.";
        break;
      case NumberParsingError::kTooLarge:
        message = "Numeric literal is too large.";
        break;
    }

    return diagnostic::DiagnosticMessage(
        diagnostic::Text(message),
        diagnostic::SourceQuote().Highlighted(range, diagnostic::Style{}));
  }

  NumberParsingError error;
  std::string_view range;
};

struct UnprintableSourceCharacter {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "unprintable-source-character";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Encountered unprintable character '\\x%02x' "
                         "encountered in source.",
                         value),
        diagnostic::SourceQuote().Highlighted(range, diagnostic::Style{}));
  }

  int value;
  std::string_view range;
};

struct InvalidSourceCharacter {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "invalid-source-character";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Invalid character '%c' encountered in source.",
                         value),
        diagnostic::SourceQuote().Highlighted(range, diagnostic::Style{}));
  }

  char value;
  std::string_view range;
};

struct StringLiteralParsingFailure {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName = "string-literal-parsing-failure";

  diagnostic::DiagnosticMessage ToMessage() const {
    // TODO: Display the contents of the errors.
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Failure to parse string literal: %d error%s.",
                         errors.size(), errors.size() == 1 ? "" : "s"),
        diagnostic::SourceQuote().Highlighted(range, diagnostic::Style{}));
  }

  std::vector<StringLiteralError> errors;
  std::string_view range;
};

struct HashtagParsingFailure {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "hashtag-parsing-failure";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(message),
        diagnostic::SourceQuote().Highlighted(range, diagnostic::Style{}));
  }

  std::string message;
  std::string_view range;
};

struct NonWhitespaceAfterNewlineEscape {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName =
      "non-whitespace-after-newline-escape";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Invalid escaped newline."),
        diagnostic::SourceQuote().Highlighted(range, diagnostic::Style{}));
  }

  std::string_view range;
};

constexpr inline bool IsLower(char c) { return ('a' <= c and c <= 'z'); }
constexpr inline bool IsUpper(char c) { return ('A' <= c and c <= 'Z'); }
constexpr inline bool IsDigit(char c) { return ('0' <= c and c <= '9'); }

constexpr inline bool IsAlpha(char c) { return IsLower(c) or IsUpper(c); }
constexpr inline bool IsAlphaNumeric(char c) {
  return IsAlpha(c) or IsDigit(c);
}
constexpr inline bool IsWhitespace(char c) {
  return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}
constexpr inline bool IsHorizontalWhitespace(char c) {
  return c == ' ' or c == '\t';
}
constexpr inline bool IsAlphaOrUnderscore(char c) {
  return IsAlpha(c) or (c == '_');
}
constexpr inline bool IsAlphaNumericOrUnderscore(char c) {
  return IsAlphaNumeric(c) or (c == '_');
}

std::string_view NextSimpleWord(std::string_view &cursor) {
  char const *start = cursor.data();
  char const *p     = cursor.data();
  while (IsAlphaNumericOrUnderscore(*p)) { ++p; }
  size_t length = p - cursor.data();
  cursor.remove_prefix(length);
  return std::string_view(start, length);
}

static base::Global kKeywords =
    absl::flat_hash_map<std::string_view, std::variant<Operator, Syntax>>{
        {"import", {Operator::Import}}, {"flags", {Syntax::Flags}},
        {"enum", {Syntax::Enum}},       {"struct", {Syntax::Struct}},
        {"return", {Operator::Return}}, {"as", {Operator::As}},
        {"copy", {Operator::Copy}},     {"init", {Operator::Init}},
        {"move", {Operator::Move}},     {"destroy", {Operator::Destroy}},
        {"and", {Operator::And}},       {"if", {Syntax::If}},
        {"while", {Syntax::While}},     {"or", {Operator::Or}},
        {"xor", {Operator::Xor}},       {"not", {Operator::Not}}};

// Consumes a character literal represented by a bang (!) followed by one
// of the following wrapped in single quotation marks:
// * A single non backslash character,
// * A backslash and then any character in the set [abfnrtv0!\]
Lexeme ConsumeCharLiteral(char const *&cursor) {
  char const *start_loc = cursor;
  ASSERT(*cursor == '!');
  // TODO: This shouldn't be an assert. it should be a genuine error we can
  // check for.
  ASSERT(cursor[1] == '\'');
  cursor += 2;
  // TODO: Ensure the character is printable.
  char c;
  if (*cursor == '\\') {
    cursor += 1;
    switch (*cursor) {
      case '\\':
      case 'a': c = '\a'; break;
      case 'b': c = '\b'; break;
      case 'f': c = '\f'; break;
      case 'n': c = '\n'; break;
      case 'r': c = '\r'; break;
      case 't': c = '\t'; break;
      case 'v': c = '\v'; break;
      case '0': c = '\0'; break;
      default: NOT_YET(); break;
    }
    cursor += 1;
  } else {
    c = *cursor;
    cursor += 1;
  }
  // TODO: This shouldn't be an assert. it should be a genuine error we can
  // check for.
  ASSERT(*cursor == '\'');
  cursor += 1;
  return Lexeme(std::make_unique<ast::Terminal>(
      std::string_view(start_loc, cursor), ir::Char(c)));
}

// Note: The order here is somewhat important. Because we choose the first
// match, we cannot, for example, put `:` before `::=`.
using elem = std::pair<std::string_view, std::variant<Operator, Syntax>>;
static base::Global kOps = std::array{
    elem{"@", {Operator::At}},           elem{"[/]", {Operator::Slice}},
    elem{"[*]", {Operator::BufPtr}},     elem{"$", {Operator::ArgType}},
    elem{"+=", {Operator::AddEq}},       elem{"+", {Operator::Add}},
    elem{"-=", {Operator::SubEq}},       elem{"..", {Operator::VariadicPack}},
    elem{"->", {Operator::Arrow}},       elem{"-", {Operator::Sub}},
    elem{"*=", {Operator::MulEq}},       elem{"*", {Operator::Mul}},
    elem{"%=", {Operator::ModEq}},       elem{"%", {Operator::Mod}},
    elem{"&=", {Operator::SymbolAndEq}}, elem{"&", {Operator::SymbolAnd}},
    elem{"|=", {Operator::SymbolOrEq}},  elem{"|", {Operator::SymbolOr}},
    elem{"^=", {Operator::SymbolXorEq}}, elem{"^", {Operator::SymbolXor}},
    elem{">>", {Operator::BlockJump}},   elem{">=", {Operator::Ge}},
    elem{">", {Operator::Gt}},           elem{"::=", {Operator::DoubleColonEq}},
    elem{":?", {Operator::TypeOf}},      elem{"::", {Operator::DoubleColon}},
    elem{":=", {Operator::ColonEq}},     elem{".", {Syntax::Dot}},
    elem{"!=", {Operator::Ne}},          elem{":", {Operator::Colon}},
    elem{"<<", {Operator::Yield}},       elem{"<=", {Operator::Le}},
    elem{"<", {Operator::Lt}},           elem{"==", {Operator::Eq}},
    elem{"=>", {Operator::Rocket}},      elem{"=", {Operator::Assign}},
    elem{"'", {Operator::Call}},         elem{"(", {Syntax::LeftParen}},
    elem{")", {Syntax::RightParen}},     elem{"[", {Syntax::LeftBracket}},
    elem{"]", {Syntax::RightBracket}},   elem{"{", {Syntax::LeftBrace}},
    elem{"}", {Syntax::RightBrace}},     elem{"~", {Operator::Tilde}},
    elem{";", {Syntax::Semicolon}},      elem{"`", {Operator::Backtick}},
    elem{",", {Operator::Comma}},
};

Lexeme NextOperator(std::string_view &cursor) {
  if (cursor.starts_with("--")) {
    cursor.remove_prefix(2);
    return Lexeme(std::make_unique<ast::Identifier>(
        std::string_view(cursor.data() - 2, 0)));
  }

  for (auto [prefix, x] : *kOps) {
    if (cursor.starts_with(prefix)) {
      std::string_view result(cursor.data(), prefix.size());
      cursor.remove_prefix(prefix.size());
      return std::visit([&](auto x) { return Lexeme(x, result); }, x);
    }
  }

  char const *loc = cursor.data();
  auto result     = ConsumeCharLiteral(loc);
  cursor.remove_prefix(loc - cursor.data());
  return result;
}

std::optional<std::pair<std::string_view, Operator>> NextSlashInitiatedToken(
    std::string_view &cursor) {
  char const *loc = cursor.data();
  cursor.remove_prefix(1);
  // TODO support multi-line comments?
  switch (cursor[0]) {
    case '/':  // line comment
      ConsumeWhile(cursor, [](char c) { return c != '\n'; });
      return std::nullopt;
    case '=':
      cursor.remove_prefix(1);
      return std::pair{std::string_view(loc, 2), Operator::DivEq};
    default: return std::pair{std::string_view(loc, 1), Operator::Div};
  }
}

static base::Global kReservedTypes = absl::flat_hash_map<std::string_view,
                                                         core::Type>{
    {"bool", semantic_analysis::Bool},       {"char", semantic_analysis::Char},
    {"i8", semantic_analysis::I(8)},         {"i16", semantic_analysis::I(16)},
    {"i32", semantic_analysis::I(32)},       {"i64", semantic_analysis::I(64)},
    {"u8", semantic_analysis::U(8)},         {"u16", semantic_analysis::U(16)},
    {"u32", semantic_analysis::U(32)},       {"u64", semantic_analysis::U(64)},
    {"f32", semantic_analysis::F32},         {"f64", semantic_analysis::F64},
    {"integer", semantic_analysis::Integer}, {"type", semantic_analysis::Type},
    {"module", semantic_analysis::Module}};

// Consumes as many alpha-numeric or underscore characters as possible, assuming
// the character under the cursor is an alpha or underscore character. Returns a
// Lexeme representing either an identifier or the builtin keyword or value for
// this word.
Lexeme ConsumeWord(std::string_view &cursor) {
  ASSERT(IsAlphaOrUnderscore(cursor[0]));

  // Because we have already verified that the character locateted at `cursor`
  // is not numeric, it is safe to consume alhpanumeric and underscore
  // characters.
  std::string_view word = ConsumeWhile(cursor, IsAlphaNumericOrUnderscore);

  if (word == "true") {
    return Lexeme(std::make_unique<ast::Terminal>(word, true));
  } else if (word == "false") {
    return Lexeme(std::make_unique<ast::Terminal>(word, false));
  } else if (word == "byte") {
    return Lexeme(
        std::make_unique<ast::Terminal>(word, semantic_analysis::Byte));
  } else if (word == "null") {
    return Lexeme(std::make_unique<ast::Terminal>(word, ir::Null()));
  } else if (word == "arguments") {
    return Lexeme(std::make_unique<ast::ProgramArguments>(word));
  } else if (word == "builtin") {
    return Lexeme(std::make_unique<ast::Builtin>(word));
  }

  if (auto iter = kReservedTypes->find(word); iter != kReservedTypes->end()) {
    return Lexeme(std::make_unique<ast::Terminal>(word, iter->second));
  }

  if (auto iter = kKeywords->find(word); iter != kKeywords->end()) {
    return std::visit([r = word](auto x) { return Lexeme(x, r); },
                      iter->second);
  }

  // TODO: Scope used to be special due it it being a keyword signifying a scope
  // and the type of such a construct, but this is no longer relevant, so we
  // don't need to treat it specially.
  if (word == "scope") { return Lexeme(Syntax::Scope, word); }
  if (word == "interface") { return Lexeme(Syntax::Interface, word); }

  return Lexeme(std::make_unique<ast::Identifier>(word));
}

struct StringLiteralLexResult {
  std::string value;
  std::string_view range;
  std::vector<StringLiteralError> errors;
};

StringLiteralLexResult NextStringLiteral(std::string_view &cursor) {
  StringLiteralLexResult result;
  cursor.remove_prefix(1);
  bool escaped                    = false;
  int offset                      = -1;
  std::string_view str_lit_cursor = ConsumeWhile(cursor, [&](char c) {
    ++offset;
    if (not escaped) {
      switch (c) {
        case '\\': escaped = true; return true;
        case '"': return false;
        // TODO non-printable chars?
        default: result.value.push_back(c); return true;
      }
    }

    escaped = false;
    switch (c) {
      case '\\':
      case '"': result.value.push_back(c); break;
      case 'a': result.value.push_back('\a'); break;
      case 'b': result.value.push_back('\b'); break;
      case 'f': result.value.push_back('\f'); break;
      case 'n': result.value.push_back('\n'); break;
      case 'r': result.value.push_back('\r'); break;
      case 't': result.value.push_back('\t'); break;
      case 'v': result.value.push_back('\v'); break;
      default: {
        result.errors.push_back(StringLiteralError{
            .kind   = StringLiteralError::Kind::kInvalidEscapedChar,
            .offset = offset,
        });
      } break;
    }

    return true;
  });

  result.range = str_lit_cursor;
  if (cursor.empty()) {
    result.errors.push_back(StringLiteralError{
        .kind   = StringLiteralError::Kind::kRunaway,
        .offset = -1,
    });
  } else {
    cursor.remove_prefix(1);  // Ending '"'
  }

  return result;
}

absl::StatusOr<Lexeme> NextHashtag(std::string_view &cursor) {
  if (cursor.empty()) {
    // TODO: use a better error code?
    return absl::InvalidArgumentError("Nothing after # character.");
  } else if (cursor[0] == '{') {
    cursor.remove_prefix(1);
    std::string_view word_cursor = NextSimpleWord(cursor);
    std::string_view token =
        std::string_view{word_cursor.data() - 1, word_cursor.size() + 2};

    if (cursor.empty() or cursor[0] != '}') {
      return absl::InvalidArgumentError(
          "Missing close brace on system hashtag.");
    }
    cursor.remove_prefix(1);

    for (auto [name, tag] : ir::BuiltinHashtagsByName) {
      if (token == name) { return Lexeme(tag, token); }
    }

    return absl::InvalidArgumentError("Unrecognized system hashtag.");
  } else if (cursor[0] == '!') {
    cursor.remove_prefix(1);
    return absl::InvalidArgumentError("Shebang directives are not supported.");
  } else {
    auto word_cursor = NextSimpleWord(cursor);

    // TODO
    return absl::InvalidArgumentError(
        "User-defined hashtags are not yet supported.");
  }
}

Lexeme ConsumeNumber(std::string_view &cursor,
                     diagnostic::DiagnosticConsumer &diag) {
  std::string_view number_str = ConsumeWhile(cursor, [](char c) {
    return IsDigit(c)
           // For hex digits, as well as 0b and 0d prefixes.
           or ('a' <= c and c <= 'f') or ('A' <= c and c <= 'F') or
           c == 'o'      // For octal
           or c == 'x'   // For hexadecimal
           or c == '_'   // For separators
           or c == '.';  // For non-integers
  });

  return std::visit(
      [&diag, number_str](auto num) {
        constexpr auto type = nth::type<decltype(num)>;
        if constexpr (type == nth::type<nth::Integer>) {
          return Lexeme(std::make_unique<ast::Terminal>(number_str, num));
        } else if constexpr (type == nth::type<double>) {
          return Lexeme(std::make_unique<ast::Terminal>(number_str, num));
        } else if constexpr (type == nth::type<NumberParsingError>) {
          // Even though we could try to be helpful by guessing the type, it's
          // unlikely to be useful. The value may also be important if it's used
          // at compile-time (e.g., as an array extent). Generally proceeding
          // further if we can't lex the input is likely not going to be useful.
          diag.Consume(NumberParsingFailure{.error = num, .range = number_str});
          return Lexeme(
              std::make_unique<ast::Terminal>(number_str, nth::Integer(0)));
        } else {
          static_assert(type.dependent(false));
        }
      },
      ParseNumber(number_str));
}

}  // namespace

std::vector<Lexeme> Lex(std::string_view content,
                        diagnostic::DiagnosticConsumer &diag) {
  std::vector<Lexeme> result;
  LexState state(content, diag);
  do { result.push_back(NextToken(&state)); } while (not result.back().eof());
  return result;
}

Lexeme NextToken(LexState *state) {
restart:
  // Delegate based on the next character in the file stream
  if (state->cursor_.empty()) {
    return Lexeme(Syntax::EndOfFile, state->cursor_);
  } else if (IsAlphaOrUnderscore(state->peek())) {
    return ConsumeWord(state->cursor_);
  } else if (IsDigit(state->peek()) or
             (state->peek() == '.' and state->cursor_.size() > 1 and
              IsDigit(state->cursor_[1]))) {
    return ConsumeNumber(state->cursor_, state->diag_);
  }

  char peek = state->peek();
  if (peek == '\n') {
    state->cursor_.remove_prefix(1);
    return Lexeme(Syntax::ImplicitNewline, state->cursor_);
  } else if (static_cast<uint8_t>(peek) >= 0x80 or
             not(std::isprint(peek) or std::isspace(peek))) {
    char const *loc = state->cursor_.data();
    state->cursor_.remove_prefix(1);
    state->diag_.Consume(UnprintableSourceCharacter{
        .value = peek,
        .range = std::string_view(loc, 1),
    });
    goto restart;
  }

  switch (peek) {
    case '"': {
      auto [str, range, errors] = NextStringLiteral(state->cursor_);
      if (not errors.empty()) {
        state->diag_.Consume(StringLiteralParsingFailure{
            .errors = std::move(errors),
            .range  = range,
        });
      }

      auto iter = GlobalStringTable.insert(std::move(str)).first;
      return Lexeme(std::make_unique<ast::Terminal>(
          range, *iter));

    } break;
    case '#': {
      state->cursor_.remove_prefix(1);
      if (state->peek() == '.') {
        state->cursor_.remove_prefix(1);
        auto word_cursor       = NextSimpleWord(state->cursor_);
        std::string_view token = word_cursor;

        return Lexeme(
            std::make_unique<ast::Label>(word_cursor, std::string{token}));

      } else {
        std::string_view loc = state->cursor_;
        auto result          = NextHashtag(state->cursor_);
        if (result.ok()) { return std::move(*result); }

        state->diag_.Consume(HashtagParsingFailure{
            .message = std::string(result.status().message()),
            .range   = std::string_view(loc.data(), state->cursor_.data()),
        });
        goto restart;
      }
    } break;
    case '/': {
      // TODO just check for comments early and roll this into NextOperator.
      if (auto maybe_op = NextSlashInitiatedToken(state->cursor_)) {
        auto &[span, op] = *maybe_op;
        return Lexeme(op, span);
      }
      goto restart;
    } break;
    case '\n':
    case '\r':
      state->cursor_.remove_prefix(1);
      return Lexeme(Syntax::ImplicitNewline, state->cursor_);
    case '\v':  // TODO: Should we disallow out vertical tabs entirely?
    case '\t':
    case ' ':
      ConsumeWhile(state->cursor_, IsHorizontalWhitespace);
      goto restart;
    case '?': {
      auto loc = state->cursor_.data();
      state->cursor_.remove_prefix(1);
      state->diag_.Consume(InvalidSourceCharacter{
          .value = peek,
          .range = std::string_view(loc, 1),
      });
      goto restart;
    }
    case '\\': {
      if (state->cursor_.size() >= 2 and state->cursor_[1] == '\\') {
        state->cursor_.remove_prefix(2);
        return Lexeme(Syntax::ExplicitNewline, state->cursor_);
      }
      state->cursor_.remove_prefix(1);
      std::string_view span = state->cursor_;
      ConsumeWhile(state->cursor_, IsWhitespace);
      if (not state->cursor_.empty()) {
        state->diag_.Consume(NonWhitespaceAfterNewlineEscape{.range = span});
      }
      goto restart;
    } break;
    default: return NextOperator(state->cursor_); break;
  }
  UNREACHABLE();
}

}  // namespace frontend
