#include <cmath>

#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "base/meta.h"
#include "core/builtin.h"
#include "error/log.h"
#include "frontend/lex.h"
#include "frontend/numbers.h"
#include "frontend/operators.h"
#include "frontend/syntax.h"
#include "ir/block_def.h"
#include "ir/results.h"
#include "ir/str.h"
#include "type/basic_type.h"
#ifdef ICARUS_MATCHER
#include "match/binding_id.h"
#include "match/binding_node.h"
#endif  // ICARUS_MATCHER

namespace frontend {

absl::flat_hash_map<std::string_view, ast::Hashtag::Builtin> const
    BuiltinHashtagMap = {{"{export}", ast::Hashtag::Builtin::Export},
                         {"{uncopyable}", ast::Hashtag::Builtin::Uncopyable},
                         {"{immovable}", ast::Hashtag::Builtin::Immovable},
                         {"{inline}", ast::Hashtag::Builtin::Inline},
                         {"{no_default}", ast::Hashtag::Builtin::NoDefault}};

namespace {

constexpr inline bool IsLower(char c) { return ('a' <= c and c <= 'z'); }
constexpr inline bool IsUpper(char c) { return ('A' <= c and c <= 'Z'); }
constexpr inline bool IsNonZeroDigit(char c) { return ('1' <= c and c <= '9'); }
constexpr inline bool IsDigit(char c) { return ('0' <= c and c <= '9'); }

constexpr inline bool IsAlpha(char c) { return IsLower(c) or IsUpper(c); }
constexpr inline bool IsAlphaNumeric(char c) {
  return IsAlpha(c) or IsDigit(c);
}
constexpr inline bool IsWhitespace(char c) {
  return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}
constexpr inline bool IsAlphaOrUnderscore(char c) {
  return IsAlpha(c) or (c == '_');
}
constexpr inline bool IsAlphaNumericOrUnderscore(char c) {
  return IsAlphaNumeric(c) or (c == '_');
}

SourceCursor NextSimpleWord(SourceCursor *cursor) {
  return cursor->ConsumeWhile(IsAlphaNumericOrUnderscore);
}

static absl::flat_hash_map<std::string_view,
                           std::variant<Operator, Syntax>> const Keywords = {
    {"which", {Operator::Which}},   {"print", {Operator::Print}},
    {"ensure", {Operator::Ensure}}, {"needs", {Operator::Needs}},
    {"import", {Operator::Import}}, {"flags", {Syntax::Flags}},
    {"enum", {Syntax::Enum}},       {"struct", {Syntax::Struct}},
    {"return", {Operator::Return}}, {"yield", {Operator::Yield}},
    {"goto", {Operator::Goto}},     {"jump", {Syntax::Jump}},
    {"switch", {Syntax::Switch}},   {"when", {Operator::When}},
    {"as", {Operator::As}},         {"copy", {Operator::Copy}},
    {"move", {Operator::Move}}};

Lexeme NextWord(SourceCursor *cursor, Source *src) {
  // Match [a-zA-Z_][a-zA-Z0-9_]*
  // We have already matched the first character
  auto word_cursor       = NextSimpleWord(cursor);
  std::string_view token = word_cursor.view();
  auto span              = word_cursor.range();

  static absl::flat_hash_map<std::string_view, type::BasicType> const
      ReservedTypes{{"bool", type::BasicType::Bool},
                    {"int8", type::BasicType::Int8},
                    {"int16", type::BasicType::Int16},
                    {"int32", type::BasicType::Int32},
                    {"int64", type::BasicType::Int64},
                    {"nat8", type::BasicType::Nat8},
                    {"nat16", type::BasicType::Nat16},
                    {"nat32", type::BasicType::Nat32},
                    {"nat64", type::BasicType::Nat64},
                    {"float32", type::BasicType::Float32},
                    {"float64", type::BasicType::Float64},
                    {"type", type::BasicType::Type_},
                    {"module", type::BasicType::Module},
                    {"byte_view", type::BasicType::ByteView}};
  // {"exit", std::pair(ir::BlockDef::Exit()}, type::BasicType::Block),
  // {"start", std::pair(ir::Results{ir::BlockDef::Start()},
  // type::BasicType::Block)},

  if (token == "true") {
    return Lexeme(std::make_unique<ast::Terminal>(std::move(span), true,
                                                  type::BasicType::Bool));
  } else if (token == "false") {
    return Lexeme(std::make_unique<ast::Terminal>(std::move(span), false,
                                                  type::BasicType::Bool));
  } else if (token == "null") {
    return Lexeme(std::make_unique<ast::Terminal>(
        std::move(span), ir::Addr::Null(), type::BasicType::NullPtr));
  }

  if (auto iter = ReservedTypes.find(token); iter != ReservedTypes.end()) {
    return Lexeme(std::make_unique<ast::Terminal>(std::move(span), iter->second,
                                                  type::BasicType::Type_));
  }
  static absl::flat_hash_map<std::string_view, core::Builtin> const BuiltinFns{
#define ICARUS_CORE_BUILTIN_X(enumerator, str, t)                              \
  {str, core::Builtin::enumerator},
#include "core/builtin.xmacro.h"
#undef ICARUS_CORE_BUILTIN_X
  };
  if (auto iter = BuiltinFns.find(token); iter != BuiltinFns.end()) {
    return Lexeme(std::make_unique<ast::BuiltinFn>(span, iter->second));
  }

  if (auto iter = Keywords.find(token); iter != Keywords.end()) {
    return std::visit([&](auto x) { return Lexeme(x, span); }, iter->second);
  }

  // "block" is special because it is also the name of the type of such a block.
  // That is, `block { ... }` has type `block`. This means that a function
  // returning a block will look like `() -> block { ... }` and there is an
  // ambiguity whereby we can't tell if this should be parsed as
  // A: () -> (block { ... }), or
  // B: (() -> block) { ... }
  //
  // We can fix this in the parser easily (by checking for a `->` beforehand
  // and prefering (B). Users can specifically add parentheses to get (A), but
  // this requires tagging "block" differently from the other block-head
  // keywords.
  if (token == "block") { return Lexeme(Syntax::Block, span); }
  if (token == "scope") { return Lexeme(Syntax::Scope, span); }

  return Lexeme(std::make_unique<ast::Identifier>(span, std::string{token}));
}

Lexeme NextNumber(SourceCursor *cursor, Source *src, error::Log *error_log) {
  auto num_cursor = cursor->ConsumeWhile([](char c) {
    return c == 'b' or c == 'o' or c == 'd' or c == 'd' or c == '_' or
           c == '.' or IsDigit(c);
  });

  auto span = num_cursor.range();
  auto num  = ParseNumber(num_cursor.view());
  if (not num.has_value()) {
    // TODO should you do something with guessing the type?
    error_log->InvalidNumber(span, num.error().to_string());
    return Lexeme(std::make_unique<ast::Terminal>(std::move(span), 0,
                                                  type::BasicType::Int32));
  }
  return std::visit(
      [&span](auto num) {
        using T = std::decay_t<decltype(num)>;
        if constexpr (std::is_same_v<T, int64_t>) {
          return Lexeme(std::make_unique<ast::Terminal>(
              std::move(span), num, type::BasicType::Int64));
        } else if constexpr (std::is_same_v<T, double>) {
          return Lexeme(std::make_unique<ast::Terminal>(
              std::move(span), num, type::BasicType::Float64));
        } else {
          static_assert(base::always_false<T>());
        }
      },
      *num);
}

std::pair<SourceRange, std::string> NextStringLiteral(SourceCursor *cursor,
                                                      Source *src,
                                                      error::Log *error_log) {
  cursor->remove_prefix(1);
  bool escaped = false;
  auto str_lit_cursor =
      cursor->ConsumeWhile([&escaped, src, error_log](char c) {
        if (c == '\\') {
          escaped = not escaped;
          return true;
        }
        if (not escaped) { return c != '"'; }
        escaped = false;
        switch (c) {
          case '"':
          case 'a':
          case 'b':
          case 'f':
          case 'n':
          case 'r':
          case 't':
          case 'v': break;
          default: {
            // TODO log an invalid escape character
            // error_log->InvalidEscapedCharacterInStringLiteral(invalid);
          } break;
        }

        return true;
      });

  auto span = str_lit_cursor.range();
  if (cursor->view().empty()) {
    error_log->RunawayStringLiteral(span);
  } else {
    cursor->remove_prefix(1);  // Ending '"'
  }

  std::string str_lit;
  // Slightly larger than necessary because we're creating extra space for
  // the
  // '\', but it's at most double the size in the most pathological case, so
  // unlikely to be an issue.
  str_lit.reserve(str_lit_cursor.view().size());
  for (auto it = str_lit_cursor.view().begin();
       it != str_lit_cursor.view().end(); ++it) {
    if (*it != '\\') {
      str_lit.push_back(*it);
      continue;
    }
    switch (*++it) {
      case '"': str_lit.push_back('"'); break;
      case 'a': str_lit.push_back('\a'); break;
      case 'b': str_lit.push_back('\b'); break;
      case 'f': str_lit.push_back('\f'); break;
      case 'n': str_lit.push_back('\n'); break;
      case 'r': str_lit.push_back('\r'); break;
      case 't': str_lit.push_back('\t'); break;
      case 'v': str_lit.push_back('\v'); break;
      default: UNREACHABLE();
    }
  }

  return std::pair{span, str_lit};
}

Lexeme NextHashtag(SourceCursor *cursor, Source *src) {
  cursor->remove_prefix(1);
  SourceRange span;
  std::string_view token;
  if (cursor->view()[0] == '{') {
    cursor->remove_prefix(1);
    auto word_cursor = NextSimpleWord(cursor);
    token            = std::string_view{word_cursor.view().data() - 1,
                             word_cursor.view().size() + 2};
    span             = word_cursor.range();

    // TODO log an error if this fails.
    ASSERT(cursor->view().size() != 0u);
    ASSERT(cursor->view()[0] == '}');
    cursor->remove_prefix(1);
    span = span.expanded(Offset(1));
  } else {
    auto word_cursor = NextSimpleWord(cursor);
    token            = word_cursor.view();
    span             = word_cursor.range();
  }

  if (auto iter = BuiltinHashtagMap.find(token);
      iter != BuiltinHashtagMap.end()) {
    return Lexeme(ast::Hashtag{iter->second}, span);
  }

  NOT_YET();
}

static bool BeginsWith(std::string_view prefix, std::string_view s) {
  if (s.size() < prefix.size()) { return false; }
  auto p_iter = prefix.begin();
  auto s_iter = s.begin();
  while (p_iter != prefix.end()) {
    if (*p_iter++ != *s_iter++) { return false; }
  }
  return true;
}

static const std::array<
    std::pair<std::string_view, std::variant<Operator, Syntax>>, 45>
    Ops = {{
        {"@", {Operator::At}},
        {",", {Operator::Comma}},
        {"[*]", {Operator::BufPtr}},
        {"$", {Operator::Eval}},
        {"+=", {Operator::AddEq}},
        {"+", {Operator::Add}},
        {"--", {Syntax::Hole}},
        {"-=", {Operator::SubEq}},
        {"->", {Operator::Arrow}},
        {"-", {Operator::Sub}},
        {"*=", {Operator::MulEq}},
        {"*", {Operator::Mul}},
        {"%=", {Operator::ModEq}},
        {"%", {Operator::Mod}},
        {"&=", {Operator::AndEq}},
        {"&", {Operator::And}},
        {"|=", {Operator::OrEq}},
        {"|", {Operator::Or}},
        {"^=", {Operator::XorEq}},
        {"^", {Operator::Xor}},
        {">=", {Operator::Ge}},
        {">", {Operator::Gt}},
        {"::=", {Operator::DoubleColonEq}},
        {"::", {Operator::DoubleColon}},
        {":=", {Operator::ColonEq}},
        {":?", {Operator::TypeOf}},
        {":", {Operator::Colon}},
        {"<<", {Operator::Expand}},
        {"..", {Operator::VariadicPack}},
        {"<=", {Operator::Le}},
        {"<", {Operator::Lt}},
        {"!=", {Operator::Ne}},
        {"!", {Operator::Not}},
        {"==", {Operator::Eq}},
        {"=>", {Operator::Rocket}},
        {"=", {Operator::Assign}},
        {"'", {Operator::Call}},
        {"(", {Syntax::LeftParen}},
        {")", {Syntax::RightParen}},
        {"[", {Syntax::LeftBracket}},
        {"]", {Syntax::RightBracket}},
        {"{", {Syntax::LeftBrace}},
        {"}", {Syntax::RightBrace}},
        {";", {Syntax::Semicolon}},
        {".", {Syntax::Dot}},
    }};

Lexeme NextOperator(SourceCursor *cursor, Source *src) {
#ifdef ICARUS_MATCHER
  // TODO "@% is a terrible choice for the operator here, but we can deal with
  // that later.
  if (BeginsWith(match::kMatchPrefix, cursor->view())) {
    cursor->remove_prefix(2);
    auto word_cursor       = NextSimpleWord(cursor);
    std::string_view token = word_cursor.view();
    auto span              = word_cursor.range();
    return Lexeme(
        std::make_unique<match::BindingNode>(match::BindingId{token}, span));
  }
#endif

  for (auto [prefix, x] : Ops) {
    if (BeginsWith(prefix, cursor->view())) {
      auto span = cursor->remove_prefix(prefix.size()).range();
      return std::visit([&](auto x) { return Lexeme(x, span); }, x);
    }
  }
  UNREACHABLE();
}

std::optional<std::pair<SourceRange, Operator>> NextSlashInitiatedToken(
    SourceCursor *cursor, Source *src, error::Log *error_log) {
  SourceRange span;
  span.begin() = cursor->loc();
  cursor->remove_prefix(1);
  switch (cursor->view()[0]) {
    case '/':  // line comment
      cursor->ConsumeWhile([](char) { return true; });
      return std::nullopt;
    case '*': {  // Multiline comment
      cursor->remove_prefix(1);
      char back_one = cursor->view()[0];
      cursor->remove_prefix(1);
      NOT_YET();

      // uint64_t comment_layer = 1;
      // while (comment_layer != 0) {
      //   if (loc.source->seen_eof) {
      //     error_log->RunawayMultilineComment();
      //     span.finish = loc.cursor;
      //     return TaggedNode(span, "", eof);

      //   } else if (back_one == '/' and *loc == '*') {
      //     ++comment_layer;

      //   } else if (back_one == '*' and *loc == '/') {
      //     --comment_layer;
      //   }

      //   back_one = *loc;
      //   cursor->remove_prefix(1);
      // }
      return std::nullopt;
    }
    case '=':
      cursor->remove_prefix(1);
      span.end().line_num = span.begin().line_num;
      span.end().offset   = span.begin().offset + 2;
      return std::pair{span, Operator::DivEq};
    default:
      span.end().line_num = span.begin().line_num;
      span.end().offset   = span.begin().offset + 1;
      return std::pair{span, Operator::Div};
  }
}
}  // namespace

Lexeme NextToken(LexState *state) {
restart:
  // Delegate based on the next character in the file stream
  if (state->cursor_.view().empty()) {
    auto chunk = state->src_->ReadUntil('\n');
    if (chunk.more_to_read) {
      state->cursor_ =
          SourceCursor(state->cursor_.loc().next_line(), chunk.view);
      return Lexeme(Syntax::ImplicitNewline,
                    state->cursor_.remove_prefix(0).range());
    } else {
      return Lexeme(Syntax::EndOfFile, state->cursor_.remove_prefix(0).range());
    }
  } else if (IsAlphaOrUnderscore(state->peek())) {
    return NextWord(&state->cursor_, state->src_);
  } else if (IsDigit(state->peek()) or
             (state->peek() == '.' and state->cursor_.view().size() > 1 and
              IsDigit(state->cursor_.view()[1]))) {
    return NextNumber(&state->cursor_, state->src_, state->error_log_);
  }
  if (BeginsWith("*/", state->cursor_.view())) {
    state->error_log_->NotInMultilineComment(
        state->cursor_.remove_prefix(2).range());
    goto restart;
  }

  switch (state->peek()) {
    case '`': {
      // auto span         = state->cursor_.remove_prefix(1).range();
      // span.end().offset = state->cursor_.loc().offset + 1;
      return Lexeme(Operator::MatchDecl, state->cursor_.range());
    } break;
    case '"': {
      auto [span, str] =
          NextStringLiteral(&state->cursor_, state->src_, state->error_log_);
      return Lexeme(std::make_unique<ast::Terminal>(std::move(span),
                                                    ir::SaveStringGlobally(str),
                                                    type::BasicType::ByteView));

    } break;
    case '#': return NextHashtag(&state->cursor_, state->src_);
    case '/': {
      // TODO just check for comments early and roll this into NextOperator.
      if (auto maybe_op = NextSlashInitiatedToken(&state->cursor_, state->src_,
                                                  state->error_log_)) {
        auto &[span, op] = *maybe_op;
        return Lexeme(op, state->cursor_.range());
      }
      goto restart;
    } break;
    case '\t':
    case ' ': state->cursor_.ConsumeWhile(IsWhitespace); goto restart;
    case '?':
      state->error_log_->InvalidCharacterQuestionMark(
          state->cursor_.remove_prefix(1).range());
      // Ignore and restart
      goto restart;
    case '\\': {
      if (state->cursor_.view().size() >= 2 and
          state->cursor_.view()[1] == '\\') {
        return Lexeme(Syntax::ExplicitNewline,
                      state->cursor_.remove_prefix(2).range());
      }
      auto span = state->cursor_.remove_prefix(1).range();
      state->cursor_.ConsumeWhile(IsWhitespace);
      if (not state->cursor_.view().empty()) {
        state->error_log_->NonWhitespaceAfterNewlineEscape(span);
      }
      goto restart;
    } break;
    default: return NextOperator(&state->cursor_, state->src_); break;
  }
  UNREACHABLE();
}

}  // namespace frontend
