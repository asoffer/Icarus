#include <unordered_map>
#include <cmath>

#include "ast/ast.h"
#include "error/log.h"
#include "frontend/numbers.h"
#include "frontend/tagged_node.h"
#include "frontend/text_span.h"
#include "type/primitive.h"

// TODO audit every location where frontend::TaggedNode::Invalid is returned to
// see if you need to log an error.

namespace frontend {
TaggedNode::TaggedNode(const TextSpan &span, const std::string &token, Tag tag)
    : node_(std::make_unique<AST::TokenNode>(span, token)), tag_(tag) {}

TaggedNode TaggedNode::TerminalExpression(const TextSpan &span, IR::Val val) {
  return TaggedNode(std::make_unique<AST::Terminal>(span, std::move(val)),
                    expr);
}
}  // namespace frontend

IR::Val ErrorFunc();
IR::Val AsciiFunc();
IR::Val OrdFunc();

namespace {
inline bool IsLower(char c) { return ('a' <= c && c <= 'z'); }
inline bool IsUpper(char c) { return ('A' <= c && c <= 'Z'); }
inline bool IsNonZeroDigit(char c) { return ('1' <= c && c <= '9'); }
inline bool IsDigit(char c) { return ('0' <= c && c <= '9'); }

template <int Base> inline i32 DigitInBase(char c);
template <> i32 DigitInBase<10>(char c) {
  return ('0' <= c && c <= '9') ? (c - '0') : -1;
}
template <> i32 DigitInBase<2>(char c) {
  return ((c | 1) == '1') ? (c - '0') : -1;
}
template <> i32 DigitInBase<8>(char c) {
  return ((c | 7) == '7') ? (c - '0') : -1;
}
template <> i32 DigitInBase<16>(char c) {
  int digit = DigitInBase<10>(c);
  if (digit != -1) { return digit; }
  if ('A' <= c && c <= 'F') { return c - 'A' + 10; }
  if ('a' <= c && c <= 'f') { return c - 'a' + 10; }
  return -1;
}

inline bool IsAlpha(char c) { return IsLower(c) || IsUpper(c); }
inline bool IsAlphaNumeric(char c) { return IsAlpha(c) || IsDigit(c); }
inline bool IsWhitespace(char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}
inline bool IsAlphaOrUnderscore(char c) { return IsAlpha(c) || (c == '_'); }
inline bool IsAlphaNumericOrUnderscore(char c) {
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
      {"true", IR::Val::Bool(true)}, {"false", IR::Val::Bool(false)},
      {"null", IR::Val::NullPtr()},  {"ord", OrdFunc()},
      {"ascii", AsciiFunc()},        {"error", ErrorFunc()},
  };
  auto iter = Reserved.find(token);
  if (iter != Reserved.end()) {
    return frontend::TaggedNode::TerminalExpression(span, iter->second);
  }

  static const std::unordered_map<std::string, frontend::Tag> KeywordMap = {
      {"in", frontend::op_b},         {"print", frontend::op_l},
      {"ensure", frontend::op_l},     {"needs", frontend::op_l},
      {"import", frontend::op_l},     {"free", frontend::op_l},
      {"for", frontend::kw_for},      {"flags", frontend::kw_block},
      {"enum", frontend::kw_block},   {"generate", frontend::op_l},
      {"struct", frontend::kw_block}, {"return", frontend::op_lt},
      {"continue", frontend::op_lt},  {"break", frontend::op_lt},
      {"repeat", frontend::op_lt},    {"restart", frontend::op_lt},
      {"scope", frontend::kw_block}};
  for (const auto & [ key, val ] : KeywordMap) {
    if (token == key) { return frontend::TaggedNode(span, key, val); }
  }

  return frontend::TaggedNode(std::make_unique<AST::Identifier>(span, token), frontend::expr);
}

template <int Base> frontend::TaggedNode NextNumberInBase(SourceLocation &loc) {
  auto span = loc.ToSpan();

  const char *start = nullptr;
  const char *dot   = nullptr;
  size_t num_dots   = 0; // 0 indicates int, 1 indicates real,
                         // anything else indicates an error.
  bool seen_zero = false;
  std::vector<i32> digits;

  while (true) {
    switch (*loc) {
    case '.':
      if (num_dots++ == 0) { dot = &*loc; }
      goto next_pre_start;
    case '0': seen_zero = true; goto next_pre_start;
    case '_': goto next_pre_start;
    default: {
      i32 digit = DigitInBase<Base>(*loc);
      if (digit == -1) { goto done_reading; }
      digits.push_back(digit);
      start = &*loc;
      goto seen_start;
    }
    }
  next_pre_start:
    loc.Increment();
  }
seen_start:
  loc.Increment();

  while (true) {
    switch (*loc) {
    case '.':
      if (num_dots++ == 0) { dot = &*loc; }
      goto next_post_start;
    case '_': goto next_post_start;
    default:
      i32 digit = DigitInBase<Base>(*loc);
      if (digit == -1) { goto done_reading; }
      digits.push_back(digit);
    }
  next_post_start:
    loc.Increment();
  }

done_reading:
  (void)dot;

  span.finish = loc.cursor;
  if (start == nullptr) {
    if (!seen_zero) {
      ErrorLog::LogGeneric(
          span,
          "TODO " __FILE__ ":" + std::to_string(__LINE__) +
              ": Found a number that has no starting digit. Treat as zero.");
    } else {
      switch (num_dots) {
      case 0: return frontend::TaggedNode::TerminalExpression(span, IR::Val::Int(0));
      default:
        ErrorLog::LogGeneric(span, "TODO " __FILE__ ":" +
                                       std::to_string(__LINE__) +
                                       ": Too many periods in numeric literal. "
                                       "Ignoring all but the first.");
        [[fallthrough]];
      case 1:
        return frontend::TaggedNode::TerminalExpression(span, IR::Val::Real(0));
      }
    }
  } else {
    switch (num_dots) {
    default:
      ErrorLog::LogGeneric(span, "TODO " __FILE__ ":" +
                                     std::to_string(__LINE__) +
                                     ": Too many periods in numeric literal. "
                                     "Ignoring all but the first.");
      [[fallthrough]];
    case 1: {
      i32 dot_offset = 0; // TODO compute this based on where the dot is
      double rep     = RepresentationAsRealInBase<Base>(digits, dot_offset);
      if (std::isnan(rep)) {
        ErrorLog::LogGeneric(span, "TODO " __FILE__ ":" +
                                       std::to_string(__LINE__) +
                                       "Number is too large to fit in a  IEEE "
                                       "754-2008 floating point number");
        return frontend::TaggedNode::TerminalExpression(span, IR::Val::Real(0));
      } else {
        return frontend::TaggedNode::TerminalExpression(span, IR::Val::Real(rep));
      }
    }
    case 0: {
      i32 rep = RepresentationAsIntInBase<Base>(digits);
      if (rep == -1) {
        ErrorLog::LogGeneric(
            span, "TODO " __FILE__ ":" + std::to_string(__LINE__) +
                      "Number is too large to fit in a 32-bit integer");
        return frontend::TaggedNode::TerminalExpression(span, IR::Val::Int(0));
      } else {
        return frontend::TaggedNode::TerminalExpression(span, IR::Val::Int(rep));
      }
    } break;
    }
  }

  // Also ignore any trailing alpha-numeric
  bool found_extra_junk = false;
  while (IsAlphaNumericOrUnderscore(*loc)) {
    found_extra_junk = true;
    loc.Increment();
  }

  if (found_extra_junk) {
    ErrorLog::LogGeneric(loc.ToSpan(), "TODO " __FILE__ ":" +
                                           std::to_string(__LINE__) + ": ");
  }

  span.finish = loc.cursor;
  return frontend::TaggedNode{};
}

frontend::TaggedNode NextZeroInitiatedNumber(SourceLocation &loc) {
  loc.Increment();

  switch (*loc) {
  case 'b': loc.Increment(); return NextNumberInBase<2>(loc);
  case 'o': loc.Increment(); return NextNumberInBase<8>(loc);
  case 'd': loc.Increment(); return NextNumberInBase<10>(loc);
  case 'x': loc.Increment(); return NextNumberInBase<16>(loc);
  default:
    if (*loc == '.') {
      return NextNumberInBase<10>(loc);
    } else if (IsAlphaNumericOrUnderscore(*loc)) {
      ErrorLog::LogGeneric(loc.ToSpan(), "TODO " __FILE__ ":" +
                                             std::to_string(__LINE__) + ": ");
      loc.BackUp();
      // TODO guess the base that was intended? If it has letters a-f, it's
      // obviously hex. If it's just 0s and 1s it's probably binary (unless it's
      // short enough?)
      return NextNumberInBase<10>(loc);
    } else {
      return frontend::TaggedNode::TerminalExpression(loc.ToSpan(), IR::Val::Int(0));
    }
  }
}

frontend::TaggedNode NextStringLiteral(SourceLocation &loc, error::Log *error_log) {
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
  return frontend::TaggedNode::TerminalExpression(span, IR::Val::StrLit(str_lit));
}

frontend::TaggedNode NextCharLiteral(SourceLocation &loc, error::Log* error_log) {
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
        return NextNumberInBase<10>(loc);
      }
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, ".", frontend::op_b);
    } else {
      if (num_dots > 2) { error_log->TooManyDots(span); }
      span.finish = loc.cursor;
      return frontend::TaggedNode(span, "..", frontend::dots);
    }
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
      auto nptr   = std::make_unique<AST::TokenNode>(span, "->");
      nptr->op    = Language::Operator::Arrow;
      return frontend::TaggedNode(std::move(nptr), frontend::fn_arrow);

    } else if (*loc == '-') {
      loc.Increment();
      span.finish = loc.cursor;
      return frontend::TaggedNode(std::make_unique<AST::Hole>(span), frontend::expr);

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

frontend::TaggedNode NextSlashInitiatedToken(SourceLocation &loc, error::Log* error_log) {
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

frontend::TaggedNode NextToken(SourceLocation &loc, error::Log *error_log) {
restart:
  // Delegate based on the next character in the file stream
  if (loc.source->seen_eof) {
    return frontend::TaggedNode(loc.ToSpan(), "", frontend::eof);
  } else if (IsAlphaOrUnderscore(*loc)) {
    return NextWord(loc);
  } else if (IsNonZeroDigit(*loc)) {
    return NextNumberInBase<10>(loc);
  }

  frontend::TaggedNode tagged_node = frontend::TaggedNode::Invalid();
  switch (*loc) {
    case '0': tagged_node = NextZeroInitiatedNumber(loc); break;
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
