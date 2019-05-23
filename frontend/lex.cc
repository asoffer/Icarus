#include <cmath>

#include "absl/container/flat_hash_map.h"
#include "ast/builtin_fn.h"
#include "ast/identifier.h"
#include "ast/terminal.h"
#include "frontend/lex.h"
#include "frontend/numbers.h"
#include "frontend/operators.h"
#include "frontend/syntax.h"
#include "ir/block.h"
#include "ir/builtin.h"
#include "ir/results.h"
#include "ir/str.h"
#include "type/primitive.h"
#include "type/util.h"
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

TextSpan ToSpan(SrcCursor const &cursor, Src *src) {
  TextSpan span;
  span.start.offset    = cursor.offset();
  span.start.line_num  = cursor.line();
  span.finish.offset   = cursor.offset() + cursor.view().size();
  span.finish.line_num = cursor.line();
  span.source          = src;
  return span;
}

constexpr inline bool IsLower(char c) { return ('a' <= c && c <= 'z'); }
constexpr inline bool IsUpper(char c) { return ('A' <= c && c <= 'Z'); }
constexpr inline bool IsNonZeroDigit(char c) { return ('1' <= c && c <= '9'); }
constexpr inline bool IsDigit(char c) { return ('0' <= c && c <= '9'); }

constexpr inline bool IsAlpha(char c) { return IsLower(c) || IsUpper(c); }
constexpr inline bool IsAlphaNumeric(char c) {
  return IsAlpha(c) || IsDigit(c);
}
constexpr inline bool IsWhitespace(char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}
constexpr inline bool IsAlphaOrUnderscore(char c) {
  return IsAlpha(c) || (c == '_');
}
constexpr inline bool IsAlphaNumericOrUnderscore(char c) {
  return IsAlphaNumeric(c) || (c == '_');
}

SrcCursor NextSimpleWord(SrcCursor *cursor) {
  return cursor->ConsumeWhile(IsAlphaNumericOrUnderscore);
}

static absl::flat_hash_map<std::string_view,
                           std::variant<Operator, Syntax>> const Keywords = {
    {"which", {Operator::Which}},       {"print", {Operator::Print}},
    {"ensure", {Operator::Ensure}},     {"needs", {Operator::Needs}},
    {"import", {Operator::Import}},     {"flags", {Syntax::Flags}},
    {"enum", {Syntax::Enum}},           {"struct", {Syntax::Struct}},
    {"return", {Operator::Return}},     {"yield", {Operator::Yield}},
    {"jump", {Operator::Jump}},         {"switch", {Syntax::Switch}},
    {"when", {Operator::When}},         {"as", {Operator::As}},
    {"interface", {Syntax::Interface}}, {"copy", {Operator::Copy}},
    {"move", {Operator::Move}}};

Lexeme NextWord(SrcCursor *cursor, Src *src) {
  // Match [a-zA-Z_][a-zA-Z0-9_]*
  // We have already matched the first character
  auto word_cursor       = NextSimpleWord(cursor);
  std::string_view token = word_cursor.view();
  auto span              = ToSpan(word_cursor, src);

  static absl::flat_hash_map<std::string_view,
                             std::pair<ir::Results, type::Type const *>> const
      Reserved{
          {"bool", std::pair(ir::Results{type::Bool}, type::Type_)},
          {"int8", std::pair(ir::Results{type::Int8}, type::Type_)},
          {"int16", std::pair(ir::Results{type::Int16}, type::Type_)},
          {"int32", std::pair(ir::Results{type::Int32}, type::Type_)},
          {"int64", std::pair(ir::Results{type::Int64}, type::Type_)},
          {"nat8", std::pair(ir::Results{type::Nat8}, type::Type_)},
          {"nat16", std::pair(ir::Results{type::Nat16}, type::Type_)},
          {"nat32", std::pair(ir::Results{type::Nat32}, type::Type_)},
          {"nat64", std::pair(ir::Results{type::Nat64}, type::Type_)},
          {"float32", std::pair(ir::Results{type::Float32}, type::Type_)},
          {"float64", std::pair(ir::Results{type::Float64}, type::Type_)},
          {"type", std::pair(ir::Results{type::Type_}, type::Type_)},
          {"module", std::pair(ir::Results{type::Module}, type::Type_)},
          {"true", std::pair(ir::Results{true}, type::Bool)},
          {"false", std::pair(ir::Results{false}, type::Bool)},
          {"null", std::pair(ir::Results{ir::Addr::Null()}, type::NullPtr)},
          {"byte_view", std::pair(ir::Results{type::ByteView}, type::Type_)},
          {"exit", std::pair(ir::Results{ir::Block::Exit()}, type::Block)},
          {"start", std::pair(ir::Results{ir::Block::Start()}, type::Block)},
      };

  if (auto iter = Reserved.find(token); iter != Reserved.end()) {
    auto const &[results, type] = iter->second;
    return Lexeme(
        std::make_unique<ast::Terminal>(span, std::move(results), type));
  }
  static absl::flat_hash_map<std::string_view, ir::Builtin> const BuiltinFns{
#define IR_BUILTIN_MACRO(enumerator, str, t) {str, ir::Builtin::enumerator},
#include "ir/builtin.xmacro.h"
#undef IR_BUILTIN_MACRO
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
  if (token == "block") {
    if (cursor->view()[0] == '?') {
      cursor->remove_prefix(1);
      span = ToSpan(word_cursor, src);
      ++span.finish.offset;
      return Lexeme(Syntax::OptBlock, span);
    } else if (cursor->view()[0] == '~') {
      cursor->remove_prefix(1);
      span = ToSpan(word_cursor, src);
      ++span.finish.offset;
      return Lexeme(Syntax::RepBlock, span);
    }
    return Lexeme(Syntax::Block, span);
  } else if (token == "scope") {
    if (cursor->view()[0] == '!') {
      cursor->remove_prefix(1);
      span = ToSpan(word_cursor, src);
      ++span.finish.offset;
      token = "scope!";
      return Lexeme(Syntax::StatefulScope, span);
    } else {
      return Lexeme(Syntax::Scope, span);
    }
  }

  return Lexeme(std::make_unique<ast::Identifier>(span, std::string{token}));
}

Lexeme NextNumber(SrcCursor *cursor, Src *src, error::Log *error_log) {
  auto num_cursor = cursor->ConsumeWhile([](char c) {
    return c == 'b' || c == 'o' || c == 'd' || c == 'd' || c == '_' ||
           c == '.' || IsDigit(c);
  });

  auto span = ToSpan(num_cursor, src);
  auto num  = ParseNumber(num_cursor.view());
  if (!num.has_value()) {
    // TODO should you do something with guessing the type?
    error_log->InvalidNumber(span, num.error().to_string());
    return Lexeme(
        std::make_unique<ast::Terminal>(span, ir::Results{0}, type::Int32));
  }
  return std::visit(
      [&span](auto x) {
        return Lexeme(std::make_unique<ast::Terminal>(
            span, ir::Results{x}, type::Get<decltype(x)>()));
      },
      *num);
}

std::pair<TextSpan, std::string> NextStringLiteral(SrcCursor *cursor, Src *src,
                                                   error::Log *error_log) {
  cursor->remove_prefix(1);
  bool escaped = false;
  auto str_lit_cursor =
      cursor->ConsumeWhile([&escaped, src, error_log](char c) {
        if (c == '\\') {
          escaped = !escaped;
          return true;
        }
        if (!escaped) { return c != '"'; }
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

  auto span = ToSpan(str_lit_cursor, src);
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

Lexeme NextHashtag(SrcCursor *cursor, Src *src) {
  cursor->remove_prefix(1);
  TextSpan span;
  std::string_view token;
  if (cursor->view()[0] == '{') {
    cursor->remove_prefix(1);
    auto word_cursor = NextSimpleWord(cursor);
    token            = std::string_view{word_cursor.view().data() - 1,
                             word_cursor.view().size() + 2};
    span             = ToSpan(word_cursor, src);

    // TODO log an error if this fails.
    ASSERT(cursor->view().size() != 0u);
    ASSERT(cursor->view()[0] == '}');
    cursor->remove_prefix(1);
    --span.start.offset;
    ++span.finish.offset;
  } else {
    auto word_cursor = NextSimpleWord(cursor);
    token            = word_cursor.view();
    span             = ToSpan(word_cursor, src);
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
    std::pair<std::string_view, std::variant<Operator, Syntax>>, 44>
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

Lexeme NextOperator(SrcCursor *cursor, Src*src) {
#ifdef ICARUS_MATCHER
  // TODO "@% is a terrible choice for the operator here, but we can deal with
  // that later.
  if (BeginsWith(match::kMatchPrefix, cursor->view())) {
    cursor->remove_prefix(2);
    auto word_cursor       = NextSimpleWord(cursor);
    std::string_view token = word_cursor.view();
    auto span = ToSpan(word_cursor, src);
    return Lexeme(
        std::make_unique<match::BindingNode>(match::BindingId{token}, span));
  }
#endif

  for (auto [prefix, x] : Ops) {
    if (BeginsWith(prefix, cursor->view())) {
      auto span = ToSpan(cursor->remove_prefix(prefix.size()), src);
      return std::visit([&](auto x) { return Lexeme(x, span); }, x);
    }
  }
  UNREACHABLE();
}

std::optional<std::pair<TextSpan, Operator>> NextSlashInitiatedToken(
    SrcCursor *cursor, Src *src, error::Log *error_log) {
  TextSpan span;
  span.start.offset   = cursor->offset();
  span.start.line_num = cursor->line();
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

      //   } else if (back_one == '/' && *loc == '*') {
      //     ++comment_layer;

      //   } else if (back_one == '*' && *loc == '/') {
      //     --comment_layer;
      //   }

      //   back_one = *loc;
      //   cursor->remove_prefix(1);
      // }
      return std::nullopt;
    }
    case '=':
      cursor->remove_prefix(1);
      span.finish.line_num = span.start.line_num;
      span.finish.offset= span.start.offset + 2;
      return std::pair{span, Operator::DivEq};
    default:
      span.finish.line_num = span.start.line_num;
      span.finish.offset   = span.start.offset + 1;
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
      state->cursor_ = SrcCursor{state->cursor_.line() + 1, 0, chunk.view};
      return Lexeme(Syntax::ImplicitNewline,
                    ToSpan(state->cursor_.remove_prefix(0), state->src_));
    } else {
      return Lexeme(Syntax::EndOfFile,
                    ToSpan(state->cursor_.remove_prefix(0), state->src_));
    }
  } else if (IsAlphaOrUnderscore(state->peek())) {
    return NextWord(&state->cursor_, state->src_);
  } else if (IsDigit(state->peek()) ||
             (state->peek() == '.' && state->cursor_.view().size() > 1 &&
              IsDigit(state->cursor_.view()[1]))) {
    return NextNumber(&state->cursor_, state->src_, state->error_log_);
  }
  if (BeginsWith("*/", state->cursor_.view())) {
    state->error_log_->NotInMultilineComment(
        ToSpan(state->cursor_.remove_prefix(2), state->src_));
    goto restart;
  }

  switch (state->peek()) {
    case '`': {
      // auto span          = ToSpan(state->cursor_.remove_prefix(1),
      // state->src_); span.finish.offset = state->cursor_.offset() + 1;
      return Lexeme(Operator::MatchDecl, ToSpan(state->cursor_, state->src_));
    } break;
    case '"': {
      auto [span, str] =
          NextStringLiteral(&state->cursor_, state->src_, state->error_log_);
      return Lexeme(std::make_unique<ast::Terminal>(
          span, ir::Results{ir::SaveStringGlobally(str)}, type::ByteView));

    } break;
    case '#': return NextHashtag(&state->cursor_, state->src_);
    case '/': {
      // TODO just check for comments early and roll this into NextOperator.
      if (auto maybe_op = NextSlashInitiatedToken(&state->cursor_, state->src_,
                                                  state->error_log_)) {
        auto &[span, op] = *maybe_op;
        return Lexeme(op, ToSpan(state->cursor_, state->src_));
      }
      goto restart;
    } break;
    case '\t':
    case ' ': state->cursor_.ConsumeWhile(IsWhitespace); goto restart;
    case '?':
      state->error_log_->InvalidCharacterQuestionMark(
          ToSpan(state->cursor_.remove_prefix(1), state->src_));
      // Ignore and restart
      goto restart;
    case '\\': {
      if (state->cursor_.view().size() >= 2 &&
          state->cursor_.view()[1] == '\\') {
        return Lexeme(Syntax::ExplicitNewline,
                      ToSpan(state->cursor_.remove_prefix(2), state->src_));
      }
      auto span = ToSpan(state->cursor_.remove_prefix(1), state->src_);
      state->cursor_.ConsumeWhile(IsWhitespace);
      if (!state->cursor_.view().empty()) {
        state->error_log_->NonWhitespaceAfterNewlineEscape(span);
      }
      goto restart;
    } break;
    default: return NextOperator(&state->cursor_, state->src_); break;
  }
  UNREACHABLE();
}

}  // namespace frontend
