#include <unordered_map>
#include <cmath>

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

IR::Val ErrorFunc();
IR::Val AsciiFunc();
IR::Val OrdFunc();

namespace frontend {
TaggedNode::TaggedNode(const TextSpan &span, const std::string &token, Tag tag)
    : node_(std::make_unique<Token>(span, token)), tag_(tag) {}

TaggedNode TaggedNode::TerminalExpression(const TextSpan &span, IR::Val val) {
  return TaggedNode(std::make_unique<AST::Terminal>(span, std::move(val)),
                    expr);
}

namespace {
constexpr inline bool IsLower(char c) { return ('a' <= c && c <= 'z'); }
constexpr inline bool IsUpper(char c) { return ('A' <= c && c <= 'Z'); }
constexpr inline bool IsNonZeroDigit(char c) { return ('1' <= c && c <= '9'); }
constexpr inline bool IsDigit(char c) { return ('0' <= c && c <= '9'); }

constexpr inline bool IsAlpha(char c) { return IsLower(c) || IsUpper(c); }
constexpr inline bool IsAlphaNumeric(char c) { return IsAlpha(c) || IsDigit(c); }
constexpr inline bool IsWhitespace(char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}
constexpr inline bool IsAlphaOrUnderscore(char c) {
  return IsAlpha(c) || (c == '_');
}
constexpr inline bool IsAlphaNumericOrUnderscore(char c) {
  return IsAlphaNumeric(c) || (c == '_');
}

frontend::TaggedNode NextWord(SourceLocation &loc) {
  // Match [a-zA-Z_][a-zA-Z0-9_]*
  // We have already matched the first character
  auto span = loc.ToSpan();
  do { loc.Increment(); } while (IsAlphaNumericOrUnderscore(*loc));
  span.finish = loc.cursor;

  std::string token = loc.line().substr(span.start.offset,
                                        span.finish.offset - span.start.offset);

  static std::unordered_map<std::string, IR::Val> Reserved{
#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  {#name, IR::Val::Type(type::GlobalName)},
#include "../type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
      {"true", IR::Val::Bool(true)},    {"false", IR::Val::Bool(false)},
      {"null", IR::Val::NullPtr()},     {"ord", OrdFunc()},
      {"ascii", AsciiFunc()},           {"error", ErrorFunc()},
      {"exit", IR::Val::Block(nullptr)}};
  if (auto iter = Reserved.find(token); iter != Reserved.end()) {
    return frontend::TaggedNode::TerminalExpression(span, iter->second);
  }

  static const std::unordered_map<std::string, frontend::Tag> KeywordMap = {
      {"print", frontend::op_l},      {"ensure", frontend::op_l},
      {"needs", frontend::op_l},      {"import", frontend::op_l},
      {"free", frontend::op_l},       {"flags", frontend::kw_block},
      {"enum", frontend::kw_block},   {"generate", frontend::op_l},
      {"struct", frontend::kw_block}, {"return", frontend::op_lt},
      {"scope", frontend::kw_block},  {"switch", frontend::kw_block},
      {"when", frontend::op_b},       {"block", frontend::kw_block}};
  if (auto iter = KeywordMap.find(token); iter != KeywordMap.end()) {
    return frontend::TaggedNode(span, iter->first, iter->second);
  }

  return frontend::TaggedNode(std::make_unique<AST::Identifier>(span, token),
                              frontend::expr);
}

frontend::TaggedNode NextNumber(SourceLocation &loc) {
  auto span = loc.ToSpan();
  const char *start = &*loc;
  while (*loc == 'b' || *loc == 'o' || *loc == 'd' || *loc == 'x' ||
         *loc == '_' || *loc == '.' || IsDigit(*loc)) {
    loc.Increment();
  }
  span.finish = loc.cursor;
  return std::visit(
      base::overloaded{[&span](i32 n) {
                         return frontend::TaggedNode::TerminalExpression(
                             span, IR::Val::Int(n));
                       },
                       [&span](double d) {
                         return frontend::TaggedNode::TerminalExpression(
                             span, IR::Val::Real(d));
                       },
                       [&span](const std::string &err) {
                         ErrorLog::LogGeneric(
                             span, "TODO " __FILE__ ":" +
                                       std::to_string(__LINE__) + ": " + err);
                         return frontend::TaggedNode{};
                       }},
      ParseNumber(std::string_view(start, &*loc - start)));
}

frontend::TaggedNode NextStringLiteral(SourceLocation &loc,
                                       error::Log *error_log) {
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

    loc.Increment(); // Iterate past '\\'
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
  return frontend::TaggedNode::TerminalExpression(span,
                                                  IR::Val::StrLit(str_lit));
}

frontend::TaggedNode NextCharLiteral(SourceLocation &loc,
                                     error::Log *error_log) {
  auto span = loc.ToSpan();
  loc.Increment();
  span.finish = loc.cursor;
  char result;

  switch (*loc) {
  case '\t':
    error_log->TabInCharacterLiteral(span);
    result = '\t';
    break;
  case ' ':
    ++span.finish.offset;
    error_log->SpaceInCharacterLiteral(span);
    result = '\0';
    break;
  case '\0':
    span.finish = loc.cursor;
    error_log->RunawayCharacterLiteral(span);
    return frontend::TaggedNode::TerminalExpression(span, IR::Val::Char('\0'));
  case '\\': {
    loc.Increment();
    switch (*loc) {
    case '\"': {
      result = '"';
      span.finish = loc.cursor;
      ++span.finish.offset;
      error_log->EscapedDoubleQuoteInCharacterLiteral(span);
    } break;
    case '\\': result = '\\'; break;
    case 'a': result  = '\a'; break;
    case 'b': result  = '\b'; break;
    case 'f': result  = '\f'; break;
    case 'n': result  = '\n'; break;
    case 'r': result  = '\r'; break;
    case 's': result  = ' '; break;
    case 't': result  = '\t'; break;
    case 'v': result  = '\v'; break;
    default:
      span.finish = loc.cursor;
      ++span.finish.offset;
      error_log->InvalidEscapedCharacterInCharacterLiteral(span);
      result = *loc;
    }
    break;
  }
  default: { result = *loc; } break;
  }
  loc.Increment();
  span.finish = loc.cursor;
  return frontend::TaggedNode::TerminalExpression(span, IR::Val::Char(result));
}

frontend::TaggedNode NextOperator(SourceLocation &loc, error::Log* error_log) {
  auto span = loc.ToSpan();
  switch (*loc) {
  case '@':
    loc.Increment();
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, "@", frontend::op_l);
  case ',':
    loc.Increment();
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, ",", frontend::comma);
  case ';':
    loc.Increment();
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, ";", frontend::semicolon);
  case '(':
    loc.Increment();
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, "(", frontend::l_paren);
  case ')':
    loc.Increment();
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, ")", frontend::r_paren);
  case '[':
    loc.Increment();
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, "[", frontend::l_bracket);
  case ']':
    loc.Increment();
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, "]", frontend::r_bracket);
  case '$':
    loc.Increment();
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, "$", frontend::op_l);

  case '{': {
    loc.Increment();
    if (*loc == '{') {
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "{{", frontend::l_double_brace);
    }
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, "{", frontend::l_brace);
  }
  case '}': {
    loc.Increment();
    if (*loc == '}') {
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "}}", frontend::r_double_brace);
    }
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, "}", frontend::r_brace);
  }

  case '.': {
    while (*loc == '.') { loc.Increment(); }
    span.finish     = loc.cursor;
    size_t num_dots = span.finish.offset - span.start.offset;

    if (num_dots == 1) {
      if (IsDigit(*loc)) {
        loc.BackUp();
        return NextNumber(loc);
      }
    } else {
     error_log->TooManyDots(span);
    }
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, ".", frontend::op_b);
  } break;

  case '\\': {
    size_t dist = 1;

    loc.Increment();
    ++dist;
    switch (*loc) {
    case '\\':
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "", frontend::newline);
      break;
    case '(':
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "\\(", frontend::l_ref);
      break;
    case '\0':
      // Ignore the following newline and retry
      loc.Increment();
      return frontend::TaggedNode::Invalid();
    case ' ':
    case '\t':
      while (IsWhitespace(*loc)) {
        loc.Increment();
        ++dist;
      }
      if (*loc == '\0') {
        loc.Increment();
        return frontend::TaggedNode::Invalid();
      }
      [[fallthrough]];
    default:
      span.finish = loc.cursor;
      ++span.finish.offset;
      // TODO this often causes the parser to fail afterwards
      error_log->NonWhitespaceAfterNewlineEscape(span);
      return frontend::TaggedNode::Invalid();
    }
  } break;

  case '+':
  case '%':
  case '<':
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
    return frontend::TaggedNode(span, token, frontend::op_b);
  } break;

  case '*':
    loc.Increment();
    if (*loc == '/') {
      loc.Increment();
      if (*loc == '/') {
        // Looking at "*//" which should be parsed as an asterisk followed by a
        // one-line comment.
        loc.BackUp();
        span.finish = loc.cursor;
        return frontend::TaggedNode(span, "*", frontend::op_b);
      } else {
        span.finish = loc.cursor;
        error_log->NotInMultilineComment(span);
        return frontend::TaggedNode::Invalid();
      }
    } else if (*loc == '=') {
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "*=", frontend::op_b);
    } else {
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "*", frontend::op_bl);
    }

  case '&': {
    loc.Increment();
    if (*loc == '=') {
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "&=", frontend::op_b);
    } else {
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "&", frontend::op_bl);
    }
  } break;

  case ':': {
    loc.Increment();

    if (*loc == '=') {
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, ":=", frontend::op_b);

    } else if (*loc == '?') {
      loc.Increment();
      // TODO does this make more sense as a right unary operator?
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, ":?", frontend::op_l);

    } else if (*loc == ':') {
      loc.Increment();

      if (*loc == '=') {
        loc.Increment();
        span.finish = loc.cursor;
        return frontend::TaggedNode(span, "::=", frontend::op_b);
      } else {
        span.finish = loc.cursor;
        return frontend::TaggedNode(span, "::", frontend::colon);
      }
    } else {
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, ":", frontend::colon);
    }
  } break;

  case '!': {
    loc.Increment();
    if (*loc == '=') {
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "!=", frontend::op_b);
    } else {
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "!", frontend::op_l);
    }
  } break;

  case '-': {
    loc.Increment();
    if (*loc == '=') {
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "-=", frontend::op_b);

    } else if (*loc == '>') {
      loc.Increment();
      span.finish = loc.cursor;
      auto nptr   = std::make_unique<Token>(span, "->");
      nptr->op    = Language::Operator::Arrow;
      return frontend::TaggedNode(std::move(nptr), frontend::fn_arrow);

    } else if (*loc == '-') {
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(std::make_unique<AST::Hole>(span),
                                  frontend::expr);
    } else {
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "-", frontend::op_bl);
    }
  } break;

  case '=': {
    loc.Increment();
    if (*loc == '=') {
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "==", frontend::op_b);

    } else if (*loc == '>') {
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "=>", frontend::op_b);

    } else {
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "=", frontend::eq);
    }
  } break;
  case '?':
    loc.Increment();
    span.finish = loc.cursor;
    error_log->InvalidCharacterQuestionMark(span);
    return frontend::TaggedNode::Invalid();
  case '~':
    loc.Increment();
    span.finish = loc.cursor;
    error_log->InvalidCharacterTilde(span);
    return frontend::TaggedNode::Invalid();
  case '\'':
    loc.Increment();
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, "'", frontend::op_bl);
  case '_': UNREACHABLE();
  default:
    UNREACHABLE("Encountered character whose value is ",
                static_cast<int>(*loc));
  }
}

frontend::TaggedNode NextSlashInitiatedToken(SourceLocation &loc,
                                             error::Log *error_log) {
  auto span = loc.ToSpan();
  loc.Increment();
  switch (*loc) {
  case '/': // line comment
    loc.SkipToEndOfLine();
    return frontend::TaggedNode::Invalid();
  case '*': { // Multiline comment
    loc.Increment();
    char back_one = *loc;
    loc.Increment();

    u64 comment_layer = 1;
    while (comment_layer != 0) {
      if (loc.source->seen_eof) {
        error_log->RunawayMultilineComment();
        span.finish = loc.cursor;
        return frontend::TaggedNode(span, "", frontend::eof);

      } else if (back_one == '/' && *loc == '*') {
        ++comment_layer;

      } else if (back_one == '*' && *loc == '/') {
        --comment_layer;
      }

      back_one = *loc;
      loc.Increment();
    }
    return frontend::TaggedNode::Invalid();
  }
  case '=':
    loc.Increment();
    span.finish = loc.cursor;
    return frontend::TaggedNode(span, "/=", frontend::op_b);
  default: span.finish = loc.cursor; return frontend::TaggedNode(span, "/", frontend::op_b);
  }
}
} // namespace

TaggedNode NextToken(SourceLocation &loc, error::Log *error_log) {
restart:
  // Delegate based on the next character in the file stream
  if (loc.source->seen_eof) {
    return frontend::TaggedNode(loc.ToSpan(), "", frontend::eof);
  } else if (IsAlphaOrUnderscore(*loc)) {
    return NextWord(loc);
  } else if (IsDigit(*loc)) {
    return NextNumber(loc);
  }

  frontend::TaggedNode tagged_node = frontend::TaggedNode::Invalid();
  switch (*loc) {
    case '`': tagged_node = NextCharLiteral(loc, error_log); break;
    case '"': tagged_node = NextStringLiteral(loc, error_log); break;
    case '/': tagged_node = NextSlashInitiatedToken(loc, error_log); break;
    case '\t':
    case ' ':
      loc.Increment();
      goto restart;  // Skip whitespace
    case '\n':
    case '\0':
      loc.Increment();
      return frontend::TaggedNode(loc.ToSpan(), "", frontend::newline);
    default: tagged_node = NextOperator(loc, error_log); break;
  }
  if (!tagged_node.valid()) { goto restart; }
  return tagged_node;
}
}  // namespace frontend
