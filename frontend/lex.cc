#include <cmath>
#include <unordered_map>

#include "ast/builtin_fn.h"
#include "ast/hole.h"
#include "ast/identifier.h"
#include "ast/terminal.h"
#include "frontend/lex.h"
#include "frontend/numbers.h"
#include "frontend/token.h"
#include "ir/builtin.h"
#include "ir/results.h"
#include "type/primitive.h"

// TODO audit every location where frontend::TaggedNode::Invalid is returned to
// see if you need to log an error.

namespace frontend {
TaggedNode::TaggedNode(const TextSpan &span, const std::string &token, Tag tag)
    : node_(std::make_unique<Token>(span, token, tag == hashtag)), tag_(tag) {}

TaggedNode TaggedNode::TerminalExpression(const TextSpan &span,
                                          ir::Results results, type::Type const* type) {
  return TaggedNode(std::make_unique<ast::Terminal>(span, std::move(results), type),
                    expr);
}

namespace {

TextSpan ToSpan(SrcCursor const &cursor, Src *src) {
  TextSpan span;
  span.start.offset    = cursor.offset();
  span.start.line_num  = cursor.line();
  span.finish.offset   = cursor.offset() + cursor.view().size();
  span.finish.line_num = cursor.line();
  span.source          = ASSERT_NOT_NULL(src);
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

TaggedNode NextWord(SrcCursor *cursor, Src *src) {
  // Match [a-zA-Z_][a-zA-Z0-9_]*
  // We have already matched the first character
  auto word_cursor       = NextSimpleWord(cursor);
  std::string_view token = word_cursor.view();
  auto span              = ToSpan(word_cursor, src);

  static std::unordered_map<std::string_view,
                            std::pair<ir::Results, type::Type const *>>
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
          {"exit",
           std::pair(
               ir::Results{std::get<ir::BlockSequence>(
                   ir::Val::Block(static_cast<ast::BlockLiteral *>(nullptr))
                       .value)},
               type::Blk())},
          // TODO these are terrible. Make them reasonable. In particular, this
          // is definitively UB.
          {"start",
           std::pair(
               ir::Results{std::get<ir::BlockSequence>(
                   ir::Val::Block(reinterpret_cast<ast::BlockLiteral *>(0x1))
                       .value)},
               type::Blk())}};

  if (auto iter = Reserved.find(token); iter != Reserved.end()) {
    auto const &[results, type] = iter->second;
    return TaggedNode::TerminalExpression(span, results, type);
  }
  static std::unordered_map<std::string_view, ir::Builtin> BuiltinFns{
#define IR_BUILTIN_MACRO(enumerator, str, t) {str, ir::Builtin::enumerator},
#include "ir/builtin.xmacro.h"
#undef IR_BUILTIN_MACRO
  };
  if (auto iter = BuiltinFns.find(token); iter != BuiltinFns.end()) {
    return TaggedNode(std::make_unique<ast::BuiltinFn>(span, iter->second),
                      expr);
  }

  // TODO rename kw_struct to more clearly indicate that it's a scope block
  // taking a an optional parenthesized list before the braces.
  static const std::unordered_map<std::string_view, Tag> KeywordMap = {
      {"which", op_l},         {"print", op_l},    {"ensure", op_l},
      {"needs", op_l},         {"import", op_l},   {"flags", kw_block_head},
      {"enum", kw_block_head}, {"generate", op_l}, {"struct", kw_struct},
      {"return", op_lt},       {"yield", op_lt},   {"switch", kw_struct},
      {"when", op_b},          {"as", op_b},       {"interface", kw_block},
      {"copy", op_l},          {"move", op_l}};
  if (auto iter = KeywordMap.find(token); iter != KeywordMap.end()) {
    return TaggedNode(span, std::string{iter->first}, iter->second);
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
    type::Type const *t = type::Blk();
    if (cursor->view()[0] == '?') {
      cursor->remove_prefix(1);
      span = ToSpan(word_cursor, src);
      ++span.finish.offset;
      t     = type::OptBlock;
      token = "block?";
    } else if (cursor->view()[0] == '~') {
      cursor->remove_prefix(1);
      span = ToSpan(word_cursor, src);
      ++span.finish.offset;
      t     = type::RepBlock;
      token = "block~";
    }
    return TaggedNode(
        std::make_unique<ast::Terminal>(span, ir::Results{t}, type::Type_),
        kw_block);
  } else if (token == "scope") {
    if (cursor->view()[0] == '!') {
      cursor->remove_prefix(1);
      span = ToSpan(word_cursor, src);
      ++span.finish.offset;
      token = "scope!";
    }
    return TaggedNode(span, std::string{token}, kw_block);
  }

  return TaggedNode(std::make_unique<ast::Identifier>(span, std::string{token}), expr);
}

TaggedNode NextNumber(SrcCursor *cursor, Src *src, error::Log *error_log) {
  auto num_cursor = cursor->ConsumeWhile([](char c) {
    return c == 'b' || c == 'o' || c == 'd' || c == 'd' || c == '_' ||
           c == '.' || IsDigit(c);
  });

  auto span = ToSpan(num_cursor, src);
  auto num  = ParseNumber(num_cursor.view());
  if (!num.has_value()) {
    // TODO should you do something with guessing the type?
    error_log->InvalidNumber(span, num.error().to_string());
    return TaggedNode::TerminalExpression(span, ir::Results{0}, type::Int32);
  }
  return std::visit(
      [&span](auto x) {
        return TaggedNode::TerminalExpression(span, ir::Results{x},
                                              type::Get<decltype(x)>());
      },
      *num);
}

TaggedNode NextStringLiteral(SrcCursor *cursor, Src *src,
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
  // Slightly larger than necessary because we're creating extra space for the
  // '\', but it's at most double the size in the most pathological case, so
  // unlikely to be an issue.
  str_lit.reserve(str_lit_cursor.view().size());
  for (auto it = str_lit_cursor.view().begin(); it != str_lit_cursor.view().end(); ++it) {
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

  return TaggedNode::TerminalExpression(
      span, ir::Results{ir::SaveStringGlobally(str_lit)}, type::ByteView);
}

TaggedNode NextHashtag(SrcCursor *cursor, Src *src, error::Log *error_log) {
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
  return TaggedNode(span, std::string{token}, hashtag);
}

TaggedNode NextOperator(SrcCursor *cursor, Src *src, error::Log *error_log) {
  switch (cursor->view()[0]) {
    case '@':
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), "@", op_l);
    case ',':
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), ",", comma);
    case ';':
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), ";", semicolon);
    case '(':
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), "(", l_paren);
    case ')':
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), ")", r_paren);
    case '[':
      if (cursor->view().size() > 3 && cursor->view()[1] == '*' &&
          cursor->view()[2] == ']') {
        return TaggedNode(ToSpan(cursor->remove_prefix(3), src), "[*]", op_l);
      } else {
        return TaggedNode(ToSpan(cursor->remove_prefix(1), src), "[",
                          l_bracket);
      }
    case ']':
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), "]", r_bracket);
    case '$':
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), "$", op_l);
    case '{':
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), "{", l_brace);
    case '}':
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), "}", r_brace);
    case '.':
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), ".", op_b);
    case '\\': {
      if (cursor->view().size() >= 2 && cursor->view()[1] == '\\') {
        return TaggedNode(ToSpan(cursor->remove_prefix(2), src), R"(\\)",
                          newline);
      }
      auto span = ToSpan(cursor->remove_prefix(1), src);
      cursor->ConsumeWhile(IsWhitespace);
      if (!cursor->view().empty()) {
        error_log->NonWhitespaceAfterNewlineEscape(span);
      }
      // TODO using invalid to skip this node. That's not the best semantics.
      return TaggedNode::Invalid();
    } break;
    case '#': return NextHashtag(cursor, src, error_log);
    case '+':
    case '%':
    case '>':
    case '|':
    case '^': {
      auto op = cursor->remove_prefix(
          (cursor->view().size() < 2 || cursor->view()[1] != '=') ? 1 : 2);
      return TaggedNode(ToSpan(op, src), std::string{op.view()}, op_b);
    }
    case '<': {  // Handles "<", "<<", and "<="
      auto op = cursor->remove_prefix(
          (cursor->view().size() < 2 ||
           (cursor->view()[1] != '=' && cursor->view()[1] != '<'))
              ? 1
              : 2);
      return TaggedNode(ToSpan(op, src), std::string{op.view()},
                        op.view() == "<<" ? op_l : op_b);
    }
    case '*':
      if (cursor->view().size() >= 2) {
        switch (cursor->view()[1]) {
          case '/':
            error_log->NotInMultilineComment(
                ToSpan(cursor->remove_prefix(2), src));
            return TaggedNode::Invalid();
          case '=':
            return TaggedNode(ToSpan(cursor->remove_prefix(2), src),
                              "*=", op_b);
        }
      }
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), "*", op_bl);
    case '&': {
      size_t len =
          (cursor->view().size() >= 2 && cursor->view()[1] == '=') ? 2 : 1;
      auto op = cursor->remove_prefix(len);
      return TaggedNode(ToSpan(op, src), std::string{op.view()},
                        len == 1 ? op_bl : op_b);
    }
    case ':':
      if (cursor->view().size() >= 3 && cursor->view()[1] == ':' &&
          cursor->view()[2] == '=') {
        return TaggedNode(ToSpan(cursor->remove_prefix(3), src), "::=", op_b);
      }

      if (cursor->view().size() >= 2) {
        switch (cursor->view()[1]) {
          case ':': {
            auto op = cursor->remove_prefix(2);
            return TaggedNode(ToSpan(op, src), "::", colon);
          }
          case '=': {
            auto op = cursor->remove_prefix(2);
            return TaggedNode(ToSpan(op, src), ":=", op_b);
          }
          case '?': {
            auto op = cursor->remove_prefix(2);
            return TaggedNode(ToSpan(op, src), ":?", op_r);
          }
        }
      }
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), ":", colon);
    case '!': {
      size_t len =
          (cursor->view().size() >= 2 && cursor->view()[1] == '=') ? 2 : 1;
      auto op = cursor->remove_prefix(len);
      return TaggedNode(ToSpan(op, src), std::string{op.view()},
                        len == 1 ? op_l : op_b);
    }
    case '-':
      if (cursor->view().size() >= 2) {
        switch (cursor->view()[1]) {
          case '>':
            return TaggedNode(ToSpan(cursor->remove_prefix(2), src), "->",
                              fn_arrow);
          case '=':
            return TaggedNode(ToSpan(cursor->remove_prefix(2), src),
                              "-=", op_b);

          case '-':
            return TaggedNode(std::make_unique<ast::Hole>(
                                  ToSpan(cursor->remove_prefix(2), src)),
                              expr);
        }
      }
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), "-", op_bl);
    case '=':
      if (cursor->view().size() >= 2) {
        switch (cursor->view()[1]) {
          case '>':
            return TaggedNode(ToSpan(cursor->remove_prefix(2), src), "=>",
                              op_b);
          case '=':
            return TaggedNode(ToSpan(cursor->remove_prefix(2), src),
                              "==", op_b);
        }
      }
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), "=", eq);
    case '?':
      error_log->InvalidCharacterQuestionMark(
          ToSpan(cursor->remove_prefix(1), src));
      return TaggedNode::Invalid();
    case '~':
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), "~", op_l);
    case '\'':
      return TaggedNode(ToSpan(cursor->remove_prefix(1), src), "'", op_bl);
    case '_': UNREACHABLE();
    default:
      UNREACHABLE("Encountered character whose value is ",
                  static_cast<int>(cursor->view()[0]));
  }
  UNREACHABLE();
}

TaggedNode NextSlashInitiatedToken(SrcCursor *cursor, Src *src,
                                   error::Log *error_log) {
  TextSpan span;
  span.start.offset   = cursor->offset();
  span.start.line_num = cursor->line();
  span.source         = src;
  cursor->remove_prefix(1);
  switch (cursor->view()[0]) {
    case '/':  // line comment
      cursor->ConsumeWhile([](char) { return true; });
      return TaggedNode::Invalid();
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
      return TaggedNode::Invalid();
    }
    case '=':
      cursor->remove_prefix(1);
      span.finish.line_num = span.start.line_num;
      span.finish.offset= span.start.offset + 2;
      return TaggedNode(span, "/=", op_b);
    default:
      span.finish.line_num = span.start.line_num;
      span.finish.offset   = span.start.offset + 1;
      return TaggedNode(span, "/", op_b);
  }
}
}  // namespace

TaggedNode NextToken(LexState *state) {
restart:
  // Delegate based on the next character in the file stream
  if (state->cursor_.view().empty()) {
    auto chunk = state->src_->ReadUntil('\n');
    if (chunk.more_to_read) {
      state->cursor_ = SrcCursor{state->cursor_.line() + 1, 0, chunk.view};
      return TaggedNode(ToSpan(state->cursor_.remove_prefix(0), state->src_),
                        "\n", newline);
    } else {
      return TaggedNode(ToSpan(state->cursor_.remove_prefix(0), state->src_),
                        "", eof);
    }
  } else if (IsAlphaOrUnderscore(state->peek())) {
    return NextWord(&state->cursor_, state->src_);
  } else if (IsDigit(state->peek()) ||
             (state->peek() == '.' && state->cursor_.view().size() > 1 &&
              IsDigit(state->cursor_.view()[1]))) {
    return NextNumber(&state->cursor_, state->src_, state->error_log_);
  }

  TaggedNode tagged_node = TaggedNode::Invalid();
  switch (state->peek()) {
    case '`': {
      auto span          = ToSpan(state->cursor_.remove_prefix(1), state->src_);
      span.finish.offset = state->cursor_.offset() + 1;
      tagged_node        = TaggedNode(span, "`", op_b);
    } break;
    case '"': {
      tagged_node =
          NextStringLiteral(&state->cursor_, state->src_, state->error_log_);
    } break;
    case '/': {
      tagged_node = NextSlashInitiatedToken(&state->cursor_, state->src_,
                                            state->error_log_);
    } break;
    case '\t':
    case ' ': state->cursor_.ConsumeWhile(IsWhitespace); goto restart;
    case '\0': {
      auto span = ToSpan(state->cursor_.remove_prefix(0), state->src_);
      return TaggedNode(span, "\n", newline);
    } break;
    default:
      tagged_node =
          NextOperator(&state->cursor_, state->src_, state->error_log_);
      break;
  }
  if (!tagged_node.valid()) { goto restart; }
  return tagged_node;
}

}  // namespace frontend
