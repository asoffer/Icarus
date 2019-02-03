#include <cmath>
#include "base/container/unordered_map.h"

#include "ast/hole.h"
#include "ast/identifier.h"
#include "ast/terminal.h"
#include "error/log.h"
#include "frontend/numbers.h"
#include "frontend/tagged_node.h"
#include "frontend/text_span.h"
#include "frontend/token.h"
#include "type/primitive.h"

// TODO audit every location where frontend::TaggedNode::Invalid is returned to
// see if you need to log an error.

ir::Val BytesFunc();
ir::Val AlignFunc();
#ifdef DBG
ir::Val DebugIrFunc();
#endif  // DBG

extern i32 ForeignFuncIndex;
extern i32 OpaqueFuncIndex;

namespace frontend {
TaggedNode::TaggedNode(const TextSpan &span, const std::string &token, Tag tag)
    : node_(std::make_unique<Token>(span, token, tag == hashtag)), tag_(tag) {}

TaggedNode TaggedNode::TerminalExpression(const TextSpan &span, ir::Val val) {
  return TaggedNode(std::make_unique<ast::Terminal>(span, std::move(val)),
                    expr);
}

namespace {
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

TextSpan NextSimpleWord(SourceLocation &loc) {
  auto span = loc.ToSpan();
  while (IsAlphaNumericOrUnderscore(*loc)) { loc.Increment(); }
  span.finish = loc.cursor;
  return span;
}

TaggedNode NextWord(SourceLocation &loc) {
  // Match [a-zA-Z_][a-zA-Z0-9_]*
  // We have already matched the first character
  auto span         = NextSimpleWord(loc);
  std::string token = loc.line().substr(span.start.offset,
                                        span.finish.offset - span.start.offset);

  static base::unordered_map<std::string, ir::Val> Reserved{
      {"bool", ir::Val(type::Bool)},
      {"int8", ir::Val(type::Int8)},
      {"int16", ir::Val(type::Int16)},
      {"int32", ir::Val(type::Int32)},
      {"int64", ir::Val(type::Int64)},
      {"nat8", ir::Val(type::Nat8)},
      {"nat16", ir::Val(type::Nat16)},
      {"nat32", ir::Val(type::Nat32)},
      {"nat64", ir::Val(type::Nat64)},
      {"float32", ir::Val(type::Float32)},
      {"float64", ir::Val(type::Float64)},
      {"type", ir::Val(type::Type_)},
      {"module", ir::Val(type::Module)},
      {"true", ir::Val(true)},
      {"false", ir::Val(false)},
      {"null", ir::Val(nullptr)},
#ifdef DBG
      {"debug_ir", DebugIrFunc()},
#endif  // DBG
      {"foreign", ir::Val::BuiltinGeneric(ForeignFuncIndex)},
      {"opaque", ir::Val::BuiltinGeneric(OpaqueFuncIndex)},
      {"byte_view", ir::Val(type::ByteView)},
      {"bytes", BytesFunc()},
      {"alignment", AlignFunc()},
      // TODO these are terrible. Make them reasonable. In particular, this is
      // definitively UB.
      {"exit", ir::Val::Block(nullptr)},
      {"start", ir::Val::Block(reinterpret_cast<ast::BlockLiteral *>(0x1))}};
  if (auto iter = Reserved.find(token); iter != Reserved.end()) {
    return TaggedNode::TerminalExpression(span, iter->second);
  }

  static const base::unordered_map<std::string, Tag> KeywordMap = {
      {"which", op_l},         {"print", op_l},    {"ensure", op_l},
      {"needs", op_l},         {"import", op_l},   {"flags", kw_block_head},
      {"enum", kw_block_head}, {"generate", op_l}, {"struct", kw_struct},
      {"return", op_lt},       {"yield", op_lt},   {"switch", kw_block_head},
      {"when", op_b},          {"as", op_b},       {"interface", kw_block},
      {"copy", op_l},          {"move", op_l}};
  if (auto iter = KeywordMap.find(token); iter != KeywordMap.end()) {
    return TaggedNode(span, iter->first, iter->second);
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
    auto t = type::Block;
    if (*loc == '?') {
      loc.Increment();
      span.finish = loc.cursor;
      t           = type::OptBlock;
      token       = "block?";
    } else if (*loc == '~') {
      loc.Increment();
      span.finish = loc.cursor;
      t           = type::RepBlock;
      token       = "block~";
    }
    return TaggedNode(std::make_unique<ast::Terminal>(span, ir::Val(t)),
                      kw_block);
  } else if (token == "scope") {
    if (*loc == '!') {
      loc.Increment();
      span.finish = loc.cursor;
      token = "scope!";
    }
    return TaggedNode(span, token, kw_block);
  }


  return TaggedNode(std::make_unique<ast::Identifier>(span, token), expr);
}

TaggedNode NextNumber(SourceLocation &loc, error::Log *error_log) {
  auto span         = loc.ToSpan();
  const char *start = &*loc;
  while (*loc == 'b' || *loc == 'o' || *loc == 'd' || *loc == 'x' ||
         *loc == '_' || *loc == '.' || IsDigit(*loc)) {
    loc.Increment();
  }
  span.finish = loc.cursor;
  return TaggedNode::TerminalExpression(
      span, std::visit(base::overloaded{[](i32 n) { return ir::Val(n); },
                                        [](double d) { return ir::Val(d); },
                                        [&](std::string_view err) {
                                          error_log->InvalidNumber(span, err);
                                          // TODO should you do something with
                                          // guessing the type?
                                          return ir::Val(0);
                                        }},
                       ParseNumber(std::string_view(start, &*loc - start))));
}

TaggedNode NextStringLiteral(SourceLocation &loc, error::Log *error_log) {
  auto span = loc.ToSpan();
  loc.Increment();

  // TODO we assign to this repeatedly which is untenable perf-wise in the
  // long-run.
  std::string str_lit = "";

  for (; *loc != '"' && *loc != '\0'; loc.Increment()) {
    if (*loc != '\\') {
      str_lit += *loc;
      continue;
    }

    loc.Increment();  // Iterate past '\\'
    span.finish = loc.cursor;
    switch (*loc) {
      case '\\': str_lit += '\\'; break;
      case '"': str_lit += '"'; break;
      case 'a': str_lit += '\a'; break;
      case 'b': str_lit += '\b'; break;
      case 'f': str_lit += '\f'; break;
      case 'n': str_lit += '\n'; break;
      case 'r': str_lit += '\r'; break;
      case 't': str_lit += '\t'; break;
      case 'v': str_lit += '\v'; break;
      default:
        TextSpan invalid = loc.ToSpan();
        --invalid.start.offset;
        ++invalid.finish.offset;
        error_log->InvalidEscapedCharacterInStringLiteral(invalid);
        str_lit += *loc;
        break;
    }
  }

  if (*loc == '\n' || *loc == '\0') {
    span.finish = loc.cursor;
    error_log->RunawayStringLiteral(span);
  } else {
    loc.Increment();
  }

  span.finish = loc.cursor;
  return TaggedNode::TerminalExpression(span, ir::Val(str_lit));
}

TaggedNode NextHashtag(SourceLocation &loc, error::Log *error_log) {
  loc.Increment();
  TextSpan span;
  if (*loc == '{') {
    loc.Increment();
    span = NextSimpleWord(loc);
    // TODO log an error if this fails.
    ASSERT(*loc == '}');
    loc.Increment();
    --span.start.offset;
    ++span.finish.offset;
  } else {
    span = NextSimpleWord(loc);
  }
  std::string token = loc.line().substr(span.start.offset,
                                        span.finish.offset - span.start.offset);
  auto t            = TaggedNode(span, token, hashtag);
  return t;
}

TaggedNode NextOperator(SourceLocation &loc, error::Log *error_log) {
  auto span = loc.ToSpan();
  switch (*loc) {
    case '@':
      loc.Increment();
      span.finish = loc.cursor;
      return TaggedNode(span, "@", op_l);
    case ',':
      loc.Increment();
      span.finish = loc.cursor;
      return TaggedNode(span, ",", comma);
    case ';':
      loc.Increment();
      span.finish = loc.cursor;
      return TaggedNode(span, ";", semicolon);
    case '(':
      loc.Increment();
      span.finish = loc.cursor;
      return TaggedNode(span, "(", l_paren);
    case ')':
      loc.Increment();
      span.finish = loc.cursor;
      return TaggedNode(span, ")", r_paren);
    case '[': {
      loc.Increment();
      auto original = loc;
      if (*loc == '*') {
        loc.Increment();
        if (*loc == ']') {
          loc.Increment();
          span.finish = loc.cursor;
          return TaggedNode(span, "[*]", op_l);
        } else {
          loc = original;
        }
      }
      span.finish = loc.cursor;
      return TaggedNode(span, "[", l_bracket);
    } break;
    case ']':
      loc.Increment();
      span.finish = loc.cursor;
      return TaggedNode(span, "]", r_bracket);
    case '$':
      loc.Increment();
      span.finish = loc.cursor;
      return TaggedNode(span, "$", op_l);

    case '{': {
      loc.Increment();
      span.finish = loc.cursor;
      return TaggedNode(span, "{", l_brace);
    }
    case '}': {
      loc.Increment();
      span.finish = loc.cursor;
      return TaggedNode(span, "}", r_brace);
    }

    case '.': {
      while (*loc == '.') { loc.Increment(); }
      span.finish     = loc.cursor;
      size_t num_dots = span.finish.offset - span.start.offset;

      if (num_dots == 1) {
        if (IsDigit(*loc)) {
          loc.BackUp();
          return NextNumber(loc, error_log);
        }
      } else {
        error_log->TooManyDots(span);
      }
      span.finish = loc.cursor;
      return TaggedNode(span, ".", op_b);
    } break;

    case '\\': {
      size_t dist = 1;

      loc.Increment();
      ++dist;
      switch (*loc) {
        case '\\':
          loc.Increment();
          span.finish = loc.cursor;
          return TaggedNode(span, "", newline);
          break;
        case '\0':
          // Ignore the following newline and retry
          loc.Increment();
          return TaggedNode::Invalid();
        case ' ':
        case '\t':
          while (IsWhitespace(*loc)) {
            loc.Increment();
            ++dist;
          }
          if (*loc == '\0') {
            loc.Increment();
            return TaggedNode::Invalid();
          }
          [[fallthrough]];
        default:
          span.finish = loc.cursor;
          ++span.finish.offset;
          // TODO this often causes the parser to fail afterwards
          error_log->NonWhitespaceAfterNewlineEscape(span);
          return TaggedNode::Invalid();
      }
    } break;
    case '#': return NextHashtag(loc, error_log);
    case '+':
    case '%':
    case '>':
    case '|':
    case '^': {
      char first_char = *loc;
      loc.Increment();

      std::string token = "X=";
      token[0]          = first_char;
      if (*loc == '=') {
        loc.Increment();
      } else {
        token = std::string(1, first_char);
      }

      span.finish = loc.cursor;
      return TaggedNode(span, token, op_b);
    } break;
    case '<': {
      char first_char = *loc;
      loc.Increment();

      std::string token;
      Tag tag = op_b;
      if (*loc == '=') {
        loc.Increment();
        token = "<=";
      } else if (*loc == '<') {
        loc.Increment();
        token = "<<";
        tag   = op_l;
      } else {
        token = "<";
      }

      span.finish = loc.cursor;
      return TaggedNode(span, token, tag);
    }
    case '*':
      loc.Increment();
      if (*loc == '/') {
        loc.Increment();
        if (*loc == '/') {
          // Looking at "*//" which should be parsed as an asterisk followed by
          // a one-line comment.
          loc.BackUp();
          span.finish = loc.cursor;
          return TaggedNode(span, "*", op_b);
        } else {
          span.finish = loc.cursor;
          error_log->NotInMultilineComment(span);
          return TaggedNode::Invalid();
        }
      } else if (*loc == '=') {
        loc.Increment();
        span.finish = loc.cursor;
        return TaggedNode(span, "*=", op_b);
      } else {
        span.finish = loc.cursor;
        return TaggedNode(span, "*", op_bl);
      }

    case '&': {
      loc.Increment();
      if (*loc == '=') {
        loc.Increment();
        span.finish = loc.cursor;
        return TaggedNode(span, "&=", op_b);
      } else {
        span.finish = loc.cursor;
        return TaggedNode(span, "&", op_bl);
      }
    } break;

    case ':': {
      loc.Increment();

      if (*loc == '=') {
        loc.Increment();
        span.finish = loc.cursor;
        return TaggedNode(span, ":=", op_b);

      } else if (*loc == '?') {
        loc.Increment();
        span.finish = loc.cursor;
        return TaggedNode(span, ":?", op_r);

      } else if (*loc == ':') {
        loc.Increment();

        if (*loc == '=') {
          loc.Increment();
          span.finish = loc.cursor;
          return TaggedNode(span, "::=", op_b);
        } else {
          span.finish = loc.cursor;
          return TaggedNode(span, "::", colon);
        }
      } else {
        span.finish = loc.cursor;
        return TaggedNode(span, ":", colon);
      }
    } break;

    case '!': {
      loc.Increment();
      if (*loc == '=') {
        loc.Increment();
        span.finish = loc.cursor;
        return TaggedNode(span, "!=", op_b);
      } else {
        span.finish = loc.cursor;
        return TaggedNode(span, "!", op_l);
      }
    } break;

    case '-': {
      loc.Increment();
      if (*loc == '=') {
        loc.Increment();
        span.finish = loc.cursor;
        return TaggedNode(span, "-=", op_b);

      } else if (*loc == '>') {
        loc.Increment();
        span.finish = loc.cursor;
        auto nptr   = std::make_unique<Token>(span, "->");
        nptr->op    = Language::Operator::Arrow;
        return TaggedNode(std::move(nptr), fn_arrow);

      } else if (*loc == '-') {
        loc.Increment();
        span.finish = loc.cursor;
        return TaggedNode(std::make_unique<ast::Hole>(span), expr);
      } else {
        span.finish = loc.cursor;
        return TaggedNode(span, "-", op_bl);
      }
    } break;

    case '=': {
      loc.Increment();
      if (*loc == '=') {
        loc.Increment();
        span.finish = loc.cursor;
        return TaggedNode(span, "==", op_b);

      } else if (*loc == '>') {
        loc.Increment();
        span.finish = loc.cursor;
        return TaggedNode(span, "=>", op_b);

      } else {
        span.finish = loc.cursor;
        return TaggedNode(span, "=", eq);
      }
    } break;
    case '?':
      loc.Increment();
      span.finish = loc.cursor;
      error_log->InvalidCharacterQuestionMark(span);
      return TaggedNode::Invalid();
    case '~':
      loc.Increment();
      span.finish = loc.cursor;
      return TaggedNode(span, "~", op_l);
    case '\'':
      loc.Increment();
      span.finish = loc.cursor;
      return TaggedNode(span, "'", op_bl);
    case '_': UNREACHABLE();
    default:
      UNREACHABLE("Encountered character whose value is ",
                  static_cast<int>(*loc));
  }
}

TaggedNode NextSlashInitiatedToken(SourceLocation &loc, error::Log *error_log) {
  auto span = loc.ToSpan();
  loc.Increment();
  switch (*loc) {
    case '/':  // line comment
      loc.SkipToEndOfLine();
      return TaggedNode::Invalid();
    case '*': {  // Multiline comment
      loc.Increment();
      char back_one = *loc;
      loc.Increment();

      u64 comment_layer = 1;
      while (comment_layer != 0) {
        if (loc.source->seen_eof) {
          error_log->RunawayMultilineComment();
          span.finish = loc.cursor;
          return TaggedNode(span, "", eof);

        } else if (back_one == '/' && *loc == '*') {
          ++comment_layer;

        } else if (back_one == '*' && *loc == '/') {
          --comment_layer;
        }

        back_one = *loc;
        loc.Increment();
      }
      return TaggedNode::Invalid();
    }
    case '=':
      loc.Increment();
      span.finish = loc.cursor;
      return TaggedNode(span, "/=", op_b);
    default: span.finish = loc.cursor; return TaggedNode(span, "/", op_b);
  }
}
}  // namespace

TaggedNode NextToken(SourceLocation &loc, error::Log *error_log) {
restart:
  // Delegate based on the next character in the file stream
  if (loc.source->seen_eof) {
    return TaggedNode(loc.ToSpan(), "", eof);
  } else if (IsAlphaOrUnderscore(*loc)) {
    return NextWord(loc);
  } else if (IsDigit(*loc)) {
    return NextNumber(loc, error_log);
  }

  TaggedNode tagged_node = TaggedNode::Invalid();
  switch (*loc) {
    case '`': {
      loc.Increment();
      auto span   = loc.ToSpan();
      tagged_node = TaggedNode(span, "`", op_b);
    } break;
    case '"': tagged_node = NextStringLiteral(loc, error_log); break;
    case '/': tagged_node = NextSlashInitiatedToken(loc, error_log); break;
    case '\t':
    case ' ':
      loc.Increment();
      goto restart;  // Skip whitespace
    case '\n':
    case '\0': loc.Increment(); return TaggedNode(loc.ToSpan(), "", newline);
    default: tagged_node = NextOperator(loc, error_log); break;
  }
  if (!tagged_node.valid()) { goto restart; }
  return tagged_node;
}
}  // namespace frontend
