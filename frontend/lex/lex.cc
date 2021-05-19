#include "frontend/lex/lex.h"

#include <cctype>
#include <cmath>

#include "absl/container/flat_hash_map.h"
#include "absl/status/statusor.h"
#include "ast/ast.h"
#include "base/meta.h"
#include "frontend/lex/numbers.h"
#include "frontend/lex/operators.h"
#include "frontend/lex/syntax.h"
#include "ir/value/builtin_fn.h"
#include "ir/value/string.h"
#include "type/primitive.h"
#ifdef ICARUS_MATCHER
#include "match/binding_id.h"
#include "match/binding_node.h"
#endif  // ICARUS_MATCHER

namespace frontend {
namespace {

struct NumberParsingFailure {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "number-parsing-failure";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    std::string_view message;
    switch (error) {
      case NumberParsingError::kUnknownBase:
        message = "Unknown base for numeric literal";
        break;
      case NumberParsingError::kTooManyDots:
        message = "Too many `.` characters in numeric literal";
        break;
      case NumberParsingError::kNoDigits:
        message = "No digits in numeric literal";
        break;
      case NumberParsingError::kInvalidDigit:
        message = "Invalid digit encountered";
        break;
      case NumberParsingError::kTooLarge:
        message = "Numeric literal is too large";
        break;
    }

    return diagnostic::DiagnosticMessage(
        diagnostic::Text("%s", message),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  NumberParsingError error;
  SourceRange range;
};

struct UnprintableSourceCharacter {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "unprintable-source-character";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(diagnostic::Text(
        "Encountered unprintable character with integral value '%d' "
        "encountered in source.",
        value));
  }

  int value;
  SourceRange range;
};

struct InvalidSourceCharacter {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "invalid-source-character";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Invalid character '%c' encountered in source.",
                         value),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  char value;
  SourceRange range;
};

struct StringLiteralParsingFailure {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName = "string-literal-parsing-failure";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    // TODO: Implement
    return diagnostic::DiagnosticMessage();
  }

  std::vector<StringLiteralError> errors;
  SourceRange range;
};

struct HashtagParsingFailure {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "hashtag-parsing-failure";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    // TODO: Highlight the source range.
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Invalid hashtag. %s", message));
  }

  std::string message;
  SourceRange range;
};

struct NonWhitespaceAfterNewlineEscape {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName =
      "non-whitespace-after-newline-escape";

  diagnostic::DiagnosticMessage ToMessage(Source const *src) const {
    // TODO: Implement
    return diagnostic::DiagnosticMessage();
  }

  SourceRange range;
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

SourceCursor NextSimpleWord(SourceCursor &cursor) {
  return cursor.ConsumeWhile(IsAlphaNumericOrUnderscore);
}

static base::Global kKeywords =
    absl::flat_hash_map<std::string_view, std::variant<Operator, Syntax>>{
        {"import", {Operator::Import}},   {"flags", {Syntax::Flags}},
        {"enum", {Syntax::Enum}},         {"struct", {Syntax::Struct}},
        {"return", {Operator::Return}},   {"interface", {Syntax::Interface}},
        {"goto", {Operator::Goto}},       {"jump", {Syntax::Jump}},
        {"as", {Operator::As}},           {"copy", {Operator::Copy}},
        {"init", {Operator::Init}},       {"move", {Operator::Move}},
        {"destroy", {Operator::Destroy}}, {"and", {Operator::And}},
        {"or", {Operator::Or}},           {"xor", {Operator::Xor}},
        {"not", {Operator::Not}}};

static bool BeginsWith(std::string_view prefix, std::string_view s) {
  if (s.size() < prefix.size()) { return false; }
  auto p_iter = prefix.begin();
  auto s_iter = s.begin();
  while (p_iter != prefix.end()) {
    if (*p_iter++ != *s_iter++) { return false; }
  }
  return true;
}

// Note: The order here is somewhat important. Because we choose the first
// match, we cannot, for example, put `:` before `::=`.
static base::Global kOps =
    std::array<std::pair<std::string_view, std::variant<Operator, Syntax>>, 47>{
        {{"@", {Operator::At}},           {",", {Operator::Comma}},
         {"[*]", {Operator::BufPtr}},     {"$", {Operator::ArgType}},
         {"+=", {Operator::AddEq}},       {"+", {Operator::Add}},
         {"-=", {Operator::SubEq}},       {"..", {Operator::VariadicPack}},
         {"->", {Operator::Arrow}},       {"-", {Operator::Sub}},
         {"*=", {Operator::MulEq}},       {"*", {Operator::Mul}},
         {"%=", {Operator::ModEq}},       {"%", {Operator::Mod}},
         {"&=", {Operator::SymbolAndEq}}, {"&", {Operator::SymbolAnd}},
         {"|=", {Operator::SymbolOrEq}},  {"|", {Operator::SymbolOr}},
         {"^=", {Operator::SymbolXorEq}}, {"^", {Operator::SymbolXor}},
         {">=", {Operator::Ge}},          {">", {Operator::Gt}},
         {"!=", {Operator::Ne}},          {"::=", {Operator::DoubleColonEq}},
         {":?", {Operator::TypeOf}},      {"::", {Operator::DoubleColon}},
         {":=", {Operator::ColonEq}},     {".", {Syntax::Dot}},
         {":", {Operator::Colon}},        {"<<", {Operator::Yield}},
         {"<=", {Operator::Le}},          {"<", {Operator::Lt}},
         {"==", {Operator::Eq}},          {"=>", {Operator::Rocket}},
         {"=", {Operator::Assign}},       {"'", {Operator::Call}},
         {"(", {Syntax::LeftParen}},      {")", {Syntax::RightParen}},
         {"[", {Syntax::LeftBracket}},    {"]", {Syntax::RightBracket}},
         {"{", {Syntax::LeftBrace}},      {"}", {Syntax::RightBrace}},
         {"~", {Operator::Tilde}},        {";", {Syntax::Semicolon}}},
    };

Lexeme NextOperator(SourceCursor &cursor) {
#ifdef ICARUS_MATCHER
  // TODO "@% is a terrible choice for the operator here, but we can deal with
  // that later.
  if (BeginsWith(match::kMatchPrefix, cursor.view())) {
    cursor.remove_prefix(2);
    auto word_cursor       = NextSimpleWord(cursor);
    std::string_view token = word_cursor.view();
    auto span              = word_cursor.range();
    return Lexeme(
        std::make_unique<match::BindingNode>(match::BindingId{token}, span));
  }
#endif

  if (BeginsWith("--", cursor.view())) {
    auto span = cursor.remove_prefix(2).range();
    return Lexeme(std::make_unique<ast::Identifier>(span, ""));
  }

  for (auto [prefix, x] : *kOps) {
    if (BeginsWith(prefix, cursor.view())) {
      auto span = cursor.remove_prefix(prefix.size()).range();
      return std::visit([&](auto x) { return Lexeme(x, span); }, x);
    }
  }
  UNREACHABLE();
}

std::optional<std::pair<SourceRange, Operator>> NextSlashInitiatedToken(
    SourceCursor &cursor) {
  SourceRange span;
  span.begin() = cursor.loc();
  cursor.remove_prefix(1);
  // TODO support multi-line comments?
  switch (cursor.view()[0]) {
    case '/':  // line comment
      cursor.ConsumeWhile([](char c) { return c != '\n'; });
      return std::nullopt;
    case '=':
      cursor.remove_prefix(1);
      span.end() = span.begin() + Offset(2);
      return std::pair{span, Operator::DivEq};
    default:
      span.end() = span.begin() + Offset(1);
      return std::pair{span, Operator::Div};
  }
}

// Consumes a character literal represented by a backtick (`) followed by one
// of:
// * A single non backslash character,
// * A backslash and then any character in the set [abfnrtv]
Lexeme ConsumeCharLiteral(SourceLoc &cursor, SourceBuffer const &buffer) {
  SourceLoc start_loc = cursor;
  ASSERT(buffer[cursor] == '`');
  cursor += Offset(1);
  // TODO: Ensure the character is printable.
  char c;
  if (buffer[cursor] == '\\') {
    cursor += Offset(1);
    switch (buffer[cursor]) {
      case '\\':
      case '`': c = '`'; break;
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
    cursor += Offset(1);
  } else {
    c = buffer[cursor];
    cursor += Offset(1);
  }
  return Lexeme(std::make_unique<ast::Terminal>(SourceRange(start_loc, cursor),
                                                ir::Value(ir::Char(c))));
}

// Note: Despite these all being primitives, we want the value-type of this map
// to be `type::Type const*` rather than `type::Primitive const *` so that when
// passed to an `ir::Value`, the write type tag is deduced.
static base::Global kReservedTypes =
    absl::flat_hash_map<std::string_view, type::Type>{
        {"bool", type::Bool},  {"char", type::Char},    {"i8", type::I8},
        {"i16", type::I16},    {"i32", type::I32},      {"i64", type::I64},
        {"u8", type::U8},      {"u16", type::U16},      {"u32", type::U32},
        {"u64", type::U64},    {"f32", type::F32},      {"f64", type::F64},
        {"type", type::Type_}, {"module", type::Module}};

// Consumes as many alpha-numeric or underscore characters as possible, assuming
// the character under the cursor is an alpha or underscore character. Returns a
// Lexeme representing either an identifier or the builtin keyword or value for
// this word.
Lexeme ConsumeWord(SourceLoc &cursor, SourceBuffer const &buffer) {
  ASSERT(IsAlphaOrUnderscore(buffer[cursor]) == true);

  // Because we have already verified that the character locateted at `cursor`
  // is not numeric, it is safe to consume alhpanumeric and underscore
  // characters.
  auto [range, word] =
      buffer.ConsumeChunkWhile(cursor, IsAlphaNumericOrUnderscore);

  if (word == "true") {
    return Lexeme(std::make_unique<ast::Terminal>(range, ir::Value(true)));
  } else if (word == "false") {
    return Lexeme(std::make_unique<ast::Terminal>(range, ir::Value(false)));
  } else if (word == "null") {
    return Lexeme(
        std::make_unique<ast::Terminal>(range, ir::Value(ir::Null())));
  }

  if (auto iter = kReservedTypes->find(word); iter != kReservedTypes->end()) {
    return Lexeme(
        std::make_unique<ast::Terminal>(range, ir::Value(iter->second)));
  }

  if (auto maybe_builtin = ir::BuiltinFn::ByName(word)) {
    return Lexeme(std::make_unique<ast::BuiltinFn>(range, *maybe_builtin));
  }

  if (auto iter = kKeywords->find(word); iter != kKeywords->end()) {
    return std::visit([r = range](auto x) { return Lexeme(x, r); },
                      iter->second);
  }

  // "block" is special because it is also the name of the type of such a
  // block. That is, `block { ... }` has type `block`. This means that a
  // function returning a block will look like `() -> block { ... }` and there
  // is an ambiguity whereby we can't tell if this should be parsed as A: ()
  // -> (block { ... }), or B: (() -> block) { ... }
  //
  // We can fix this in the parser easily (by checking for a `->` beforehand
  // and prefering (B). Users can specifically add parentheses to get (A), but
  // this requires tagging "block" differently from the other block-head
  // keywords.
  if (word == "block") { return Lexeme(Syntax::Block, range); }
  if (word == "scope") { return Lexeme(Syntax::Scope, range); }

  return Lexeme(std::make_unique<ast::Identifier>(range, std::string{word}));
}

struct StringLiteralLexResult {
  std::string value;
  SourceRange range;
  std::vector<StringLiteralError> errors;
};

StringLiteralLexResult NextStringLiteral(SourceCursor &cursor) {
  StringLiteralLexResult result;
  cursor.remove_prefix(1);
  bool escaped        = false;
  int offset          = -1;
  auto str_lit_cursor = cursor.ConsumeWhile([&](char c) {
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

  result.range = str_lit_cursor.range();
  if (cursor.view().empty()) {
    result.errors.push_back(StringLiteralError{
        .kind   = StringLiteralError::Kind::kRunaway,
        .offset = -1,
    });
  } else {
    cursor.remove_prefix(1);  // Ending '"'
  }

  return result;
}

absl::StatusOr<Lexeme> NextHashtag(SourceCursor &cursor) {
  SourceRange span;
  std::string_view token;
  if (cursor.view().empty()) {
    // TODO: use a better error code?
    return absl::InvalidArgumentError("Nothing after # character.");
  } else if (cursor.view()[0] == '{') {
    cursor.remove_prefix(1);
    auto word_cursor = NextSimpleWord(cursor);
    token            = std::string_view{word_cursor.view().data() - 1,
                             word_cursor.view().size() + 2};
    span             = word_cursor.range();

    if (cursor.view().empty() or cursor.view()[0] != '}') {
      return absl::InvalidArgumentError(
          "Missing close brace on system hashtag.");
    }
    cursor.remove_prefix(1);
    span = span.expanded(Offset(1));

    for (auto [name, tag] : ir::BuiltinHashtagsByName) {
      if (token == name) { return Lexeme(tag, span); }
    }

    return absl::InvalidArgumentError("Unrecognized hashtag.");
  } else {
    auto word_cursor = NextSimpleWord(cursor);
    token            = word_cursor.view();
    span             = word_cursor.range();

    // TODO
    return absl::InvalidArgumentError(
        "User-defined hashtags are not yet supported.");
  }
}

Lexeme ConsumeNumber(SourceLoc &cursor, SourceBuffer const &buffer,
                     diagnostic::DiagnosticConsumer &diag) {
  auto [range, number_str] = buffer.ConsumeChunkWhile(cursor, [](char c) {
    return IsDigit(c)    //
           or c == 'b'   // For binary
           or c == 'd'   // For decimal
           or c == 'o'   // For octal
           or c == 'x'   // For hexadecimal
           or c == '_'   // For separators
           or c == '.';  // For non-integers
  });

  return std::visit(
      [&diag, r = range](auto num) {
        constexpr auto type = base::meta<decltype(num)>;
        if constexpr (type == base::meta<int64_t>) {
          return Lexeme(std::make_unique<ast::Terminal>(r, ir::Value(num)));
        } else if constexpr (type == base::meta<double>) {
          return Lexeme(std::make_unique<ast::Terminal>(r, ir::Value(num)));
        } else if constexpr (type == base::meta<NumberParsingError>) {
          // Even though we could try to be helpful by guessing the type, it's
          // unlikely to be useful. The value may also be important if it's used
          // at compile-time (e.g., as an array extent). Generally proceeding
          // further if we can't lex the input is likely not going to be useful.
          diag.Consume(NumberParsingFailure{
              .error = num,
              .range = r,
          });
          return Lexeme(std::make_unique<ast::Terminal>(r, ir::Value(0)));
        } else {
          static_assert(base::always_false(type));
        }
      },
      ParseNumber(number_str));
}
}  // namespace

std::vector<Lexeme> Lex(SourceBuffer &buffer, diagnostic::DiagnosticConsumer &diag,
                        size_t chunk) {
  std::vector<Lexeme> result;
  LexState state(&buffer, diag, chunk);
  do { result.push_back(NextToken(&state)); } while (not result.back().eof());
  return result;
}

Lexeme NextToken(LexState *state) {
restart:
  // Delegate based on the next character in the file stream
  SourceLoc loc      = state->cursor_.loc();
  std::string_view v = state->cursor_.view();
  if (state->cursor_.view().empty()) {
    return Lexeme(Syntax::EndOfFile, state->cursor_.remove_prefix(0).range());
  } else if (IsAlphaOrUnderscore(state->peek())) {
    auto result = ConsumeWord(loc, state->buffer_);
    v.remove_prefix((loc - state->cursor_.loc()).value);
    state->cursor_ = SourceCursor(loc, v);
    return result;
  } else if (IsDigit(state->peek()) or
             (state->peek() == '.' and state->cursor_.view().size() > 1 and
              IsDigit(state->cursor_.view()[1]))) {
    auto result = ConsumeNumber(loc, state->buffer_, state->diag_);
    v.remove_prefix((loc - state->cursor_.loc()).value);
    state->cursor_ = SourceCursor(loc, v);
    return result;
  }

  char peek = state->peek();
  if (peek == '\n') {
    return Lexeme(Syntax::ImplicitNewline,
                  state->cursor_.remove_prefix(1).range());
  } else if (static_cast<uint8_t>(peek) >= 0x80 or not std::isprint(peek)) {
    auto loc = state->cursor_.loc();
    state->cursor_.remove_prefix(1);
    state->diag_.Consume(UnprintableSourceCharacter{
        .value = peek,
        .range = SourceRange(loc, loc + Offset(1)),
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

      return Lexeme(std::make_unique<ast::Terminal>(
          range, ir::Value(ir::String(str).addr())));

    } break;
    case '`': {
      SourceLoc loc      = state->cursor_.loc();
      std::string_view v = state->cursor_.view();
      auto result        = ConsumeCharLiteral(loc, state->buffer_);
      v.remove_prefix((loc - state->cursor_.loc()).value);
      state->cursor_ = SourceCursor(loc, v);
      return result;
    } break;
    case '#': {
      state->cursor_.remove_prefix(1);
      if (state->cursor_.view().empty()) {
        state->diag_.Consume(
            HashtagParsingFailure{.message = "Nothing after # character."});
        goto restart;
      }
      if (state->peek() == '.') {
        state->cursor_.remove_prefix(1);
        auto word_cursor       = NextSimpleWord(state->cursor_);
        std::string_view token = word_cursor.view();

        return Lexeme(std::make_unique<ast::Label>(word_cursor.range(),
                                                   std::string{token}));

      } else {
        auto result = NextHashtag(state->cursor_);
        if (result.ok()) { return std::move(*result); }

        state->diag_.Consume(HashtagParsingFailure{
            .message = std::string(result.status().message()),
        });
        goto restart;
      }
    } break;
    case '/': {
      // TODO just check for comments early and roll this into NextOperator.
      if (auto maybe_op = NextSlashInitiatedToken(state->cursor_)) {
        auto &[span, op] = *maybe_op;
        return Lexeme(op, state->cursor_.range());
      }
      goto restart;
    } break;
    case '\n':
    case '\r':
      return Lexeme(Syntax::ImplicitNewline,
                    state->cursor_.remove_prefix(1).range());
    case '\v':  // TODO: Should we disallow out vertical tabs entirely?
    case '\t':
    case ' ': state->cursor_.ConsumeWhile(IsHorizontalWhitespace); goto restart;
    case '?': {
      auto loc = state->cursor_.loc();
      state->cursor_.remove_prefix(1);
      state->diag_.Consume(InvalidSourceCharacter{
          .value = peek,
          .range = SourceRange(loc, loc + Offset(1)),
      });
      goto restart;
    }
    case '\\': {
      if (state->cursor_.view().size() >= 2 and
          state->cursor_.view()[1] == '\\') {
        return Lexeme(Syntax::ExplicitNewline,
                      state->cursor_.remove_prefix(2).range());
      }
      auto span = state->cursor_.remove_prefix(1).range();
      state->cursor_.ConsumeWhile(IsWhitespace);
      if (not state->cursor_.view().empty()) {
        state->diag_.Consume(NonWhitespaceAfterNewlineEscape{.range = span});
      }
      goto restart;
    } break;
    default: return NextOperator(state->cursor_); break;
  }
  UNREACHABLE();
}

}  // namespace frontend
