#include <unordered_map>
#include <cmath>

#include "ast/ast.h"
#include "error/log.h"
#include "nnt.h"
#include "type/primitive.h"

// TODO audit every location where NNT::Invalid is returned to see if you need
// to log an error.

NNT::NNT(const TextSpan &span, const std::string &token, Language::NodeType nt)
    : node(std::make_unique<AST::TokenNode>(span, token)), node_type(nt) {}

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

NNT NextWord(SourceLocation &loc) {
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
    return NNT::TerminalExpression(span, iter->second);
  }

  static const std::unordered_map<std::string, Language::NodeType> KeywordMap =
      {{"in", Language::op_b},           {"print", Language::op_l},
       {"ensure", Language::op_l},       {"needs", Language::op_l},
       {"require", Language::op_l},      {"free", Language::op_l},
       {"for", Language::kw_expr_block}, {"case", Language::kw_block},
       {"flags", Language::kw_block},    {"enum", Language::kw_block},
       {"generate", Language::op_l},     {"struct", Language::kw_struct},
       {"return", Language::op_lt},      {"continue", Language::op_lt},
       {"break", Language::op_lt},       {"repeat", Language::op_lt},
       {"restart", Language::op_lt},     {"scope", Language::kw_struct}};
  for (const auto & [ key, val ] : KeywordMap) {
    if (token == key) { return NNT(span, key, val); }
  }

  return NNT(std::make_unique<AST::Identifier>(span, token), Language::expr);
}

template <int Base> inline int pow(i32 num) {
  // TODO repeated squaring
  int result = 1;
  for (int i = 0; i < num; ++i) { result *= Base; }
  return result;
}

template <int Base> bool RepresentableAsIntInBase(const std::vector<i32> &);

template <> bool RepresentableAsIntInBase<2>(const std::vector<i32> &digits) {
  return digits.size() < 32;
}

template <> bool RepresentableAsIntInBase<8>(const std::vector<i32> &digits) {
  return digits.size() < 11 || (digits.size() == 11 && digits[0] < 2);
}

template <> bool RepresentableAsIntInBase<16>(const std::vector<i32> &digits) {
  return digits.size() < 8 || (digits.size() == 8 && digits[0] < 8);
}

template <> bool RepresentableAsIntInBase<10>(const std::vector<i32> &digits) {
  return !(digits.size() > 10) &&
         (digits.size() < 10 ||
          digits < std::vector<i32>{2, 1, 4, 7, 4, 8, 3, 6, 4, 8});
}

template <int Base>
i32 RepresentationAsIntInBase(const std::vector<i32> &digits) {
  if (!RepresentableAsIntInBase<Base>(digits)) { return -1; }
  i32 result = 0;
  for (i32 digit : digits) {
    ASSERT_LT(digit, Base);
    result = result * Base + digit;
  }
  return result;
}

template <int Base>
double RepresentationAsRealInBase(const std::vector<i32> &, i32) {
  // TODO
  return NAN;
}

template <>
double RepresentationAsRealInBase<2>(const std::vector<i32> &digits,
                                     i32 dot_offset) {
  if (digits.size() > 52) { return NAN; }
  if (dot_offset > 1023 || dot_offset < -1022) { return NAN; }
  NOT_YET();
}

template <>
double RepresentationAsRealInBase<16>(const std::vector<i32> &digits,
                                      i32 dot_offset) {
  if (digits.size() > 13) { return NAN; }
  i32 exponent = dot_offset * 4 - 3;
  if (dot_offset >= 0) { exponent += 4; }
  if (digits[0] >= 8) { ++exponent; }
  if (digits[0] >= 4) { ++exponent; }
  if (digits[0] >= 2) { ++exponent; }

  if (exponent > 1023 || exponent < -1022) { return NAN; }
  NOT_YET();
}

template <>
double RepresentationAsRealInBase<8>(const std::vector<i32> &digits,
                                     i32 dot_offset) {
  if (digits.size() > 13) { return NAN; }
  i32 exponent = dot_offset * 3 - 2;
  if (dot_offset >= 0) { exponent += 3; }
  if (digits[0] >= 2) { ++exponent; }
  if (digits[0] >= 4) { ++exponent; }

  if (exponent > 1023 || exponent < -1022) { return NAN; }
  NOT_YET();
}
template <int Base> NNT NextNumberInBase(SourceLocation &loc) {
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
      case 0: return NNT::TerminalExpression(span, IR::Val::Int(0));
      default:
        ErrorLog::LogGeneric(span, "TODO " __FILE__ ":" +
                                       std::to_string(__LINE__) +
                                       ": Too many periods in numeric literal. "
                                       "Ignoring all but the first.");
        [[fallthrough]];
      case 1: return NNT::TerminalExpression(span, IR::Val::Real(0));
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
        return NNT::TerminalExpression(span, IR::Val::Real(0));
      } else {
        return NNT::TerminalExpression(span, IR::Val::Real(rep));
      }
    }
    case 0: {
      i32 rep = RepresentationAsIntInBase<Base>(digits);
      if (rep == -1) {
        ErrorLog::LogGeneric(
            span, "TODO " __FILE__ ":" + std::to_string(__LINE__) +
                      "Number is too large to fit in a 32-bit integer");
        return NNT::TerminalExpression(span, IR::Val::Int(0));
      } else {
        return NNT::TerminalExpression(span, IR::Val::Int(rep));
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
  return NNT{};
}

NNT NextZeroInitiatedNumber(SourceLocation &loc) {
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
      return NNT::TerminalExpression(loc.ToSpan(), IR::Val::Int(0));
    }
  }
}

NNT NextStringLiteral(SourceLocation &loc, error::Log *error_log) {
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
  return NNT::TerminalExpression(span, IR::Val::StrLit(str_lit));
}

NNT NextCharLiteral(SourceLocation &loc, error::Log* error_log) {
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
    return NNT::TerminalExpression(span, IR::Val::Char('\0'));
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
  return NNT::TerminalExpression(span, IR::Val::Char(result));
}

NNT NextOperator(SourceLocation &loc, error::Log* error_log) {
  auto span = loc.ToSpan();
  switch (*loc) {
  case '@':
    loc.Increment();
    span.finish = loc.cursor;
    return NNT(span, "@", Language::op_l);
  case ',':
    loc.Increment();
    span.finish = loc.cursor;
    return NNT(span, ",", Language::comma);
  case ';':
    loc.Increment();
    span.finish = loc.cursor;
    return NNT(span, ";", Language::semicolon);
  case '(':
    loc.Increment();
    span.finish = loc.cursor;
    return NNT(span, "(", Language::l_paren);
  case ')':
    loc.Increment();
    span.finish = loc.cursor;
    return NNT(span, ")", Language::r_paren);
  case '[':
    loc.Increment();
    span.finish = loc.cursor;
    return NNT(span, "[", Language::l_bracket);
  case ']':
    loc.Increment();
    span.finish = loc.cursor;
    return NNT(span, "]", Language::r_bracket);
  case '$':
    loc.Increment();
    span.finish = loc.cursor;
    return NNT(span, "$", Language::op_l);

  case '{': {
    loc.Increment();
    if (*loc == '{') {
      loc.Increment();
      span.finish = loc.cursor;
      return NNT(span, "{{", Language::l_double_brace);
    }
    span.finish = loc.cursor;
    return NNT(span, "{", Language::l_brace);
  }
  case '}': {
    loc.Increment();
    if (*loc == '}') {
      loc.Increment();
      span.finish = loc.cursor;
      return NNT(span, "}}", Language::r_double_brace);
    }
    span.finish = loc.cursor;
    return NNT(span, "}", Language::r_brace);
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
      return NNT(span, ".", Language::op_b);
    } else {
      if (num_dots > 2) { error_log->TooManyDots(span); }
      span.finish = loc.cursor;
      return NNT(span, "..", Language::dots);
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
      return NNT(span, "", Language::newline);
      break;
    case '(':
      loc.Increment();
      span.finish = loc.cursor;
      return NNT(span, "\\(", Language::l_ref);
      break;
    case '\0':
      // Ignore the following newline and retry
      loc.Increment();
      return NNT::Invalid();
    case ' ':
    case '\t':
      while (IsWhitespace(*loc)) {
        loc.Increment();
        ++dist;
      }
      if (*loc == '\0') {
        loc.Increment();
        return NNT::Invalid();
      }
      [[fallthrough]];
    default:
      span.finish = loc.cursor;
      ++span.finish.offset;
      // TODO this often causes the parser to fail afterwards
      error_log->NonWhitespaceAfterNewlineEscape(span);
      return NNT::Invalid();
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
    return NNT(span, token, Language::op_b);
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
        return NNT(span, "*", Language::op_b);
      } else {
        span.finish = loc.cursor;
        error_log->NotInMultilineComment(span);
        return NNT::Invalid();
      }
    } else if (*loc == '=') {
      loc.Increment();
      span.finish = loc.cursor;
      return NNT(span, "*=", Language::op_b);
    } else {
      span.finish = loc.cursor;
      return NNT(span, "*", Language::op_bl);
    }

  case '&': {
    loc.Increment();
    if (*loc == '=') {
      loc.Increment();
      span.finish = loc.cursor;
      return NNT(span, "&=", Language::op_b);
    } else {
      span.finish = loc.cursor;
      return NNT(span, "&", Language::op_bl);
    }
  } break;

  case ':': {
    loc.Increment();

    if (*loc == '=') {
      loc.Increment();
      span.finish = loc.cursor;
      return NNT(span, ":=", Language::op_b);

    } else if (*loc == '?') {
      loc.Increment();
      // TODO does this make more sense as a right unary operator?
      span.finish = loc.cursor;
      return NNT(span, ":?", Language::op_l);

    } else if (*loc == ':') {
      loc.Increment();

      if (*loc == '=') {
        loc.Increment();
        span.finish = loc.cursor;
        return NNT(span, "::=", Language::op_b);
      } else {
        span.finish = loc.cursor;
        return NNT(span, "::", Language::colon);
      }
    } else {
      span.finish = loc.cursor;
      return NNT(span, ":", Language::colon);
    }
  } break;

  case '!': {
    loc.Increment();
    if (*loc == '=') {
      loc.Increment();
      span.finish = loc.cursor;
      return NNT(span, "!=", Language::op_b);
    } else {
      span.finish = loc.cursor;
      return NNT(span, "!", Language::op_l);
    }
  } break;

  case '-': {
    loc.Increment();
    if (*loc == '=') {
      loc.Increment();
      span.finish = loc.cursor;
      return NNT(span, "-=", Language::op_b);

    } else if (*loc == '>') {
      loc.Increment();
      span.finish = loc.cursor;
      auto nptr   = std::make_unique<AST::TokenNode>(span, "->");
      nptr->op    = Language::Operator::Arrow;
      return NNT(std::move(nptr), Language::fn_arrow);

    } else if (*loc == '-') {
      loc.Increment();
      span.finish = loc.cursor;
      return NNT(std::make_unique<AST::Hole>(span), Language::expr);

    } else {
      span.finish = loc.cursor;
      return NNT(span, "-", Language::op_bl);
    }
  } break;

  case '=': {
    loc.Increment();
    if (*loc == '=') {
      loc.Increment();
      span.finish = loc.cursor;
      return NNT(span, "==", Language::op_b);

    } else if (*loc == '>') {
      loc.Increment();
      span.finish = loc.cursor;
      return NNT(span, "=>", Language::op_b);

    } else {
      span.finish = loc.cursor;
      return NNT(span, "=", Language::eq);
    }
  } break;
  case '?':
    loc.Increment();
    span.finish = loc.cursor;
    error_log->InvalidCharacterQuestionMark(span);
    return NNT::Invalid();
  case '~':
    loc.Increment();
    span.finish = loc.cursor;
    error_log->InvalidCharacterTilde(span);
    return NNT::Invalid();
  case '\'':
    loc.Increment();
    span.finish = loc.cursor;
    return NNT(span, "'", Language::op_bl);
  case '_': UNREACHABLE();
  default:
    UNREACHABLE("Encountered character whose value is ",
                static_cast<int>(*loc));
  }
}

NNT NextSlashInitiatedToken(SourceLocation &loc, error::Log* error_log) {
  auto span = loc.ToSpan();
  loc.Increment();
  switch (*loc) {
  case '/': // line comment
    loc.SkipToEndOfLine();
    return NNT::Invalid();
  case '*': { // Multiline comment
    loc.Increment();
    char back_one = *loc;
    loc.Increment();

    u64 comment_layer = 1;
    while (comment_layer != 0) {
      if (loc.source->seen_eof) {
        error_log->RunawayMultilineComment();
        span.finish = loc.cursor;
        return NNT(span, "", Language::eof);

      } else if (back_one == '/' && *loc == '*') {
        ++comment_layer;

      } else if (back_one == '*' && *loc == '/') {
        --comment_layer;
      }

      back_one = *loc;
      loc.Increment();
    }
    return NNT::Invalid();
  }
  case '=':
    loc.Increment();
    span.finish = loc.cursor;
    return NNT(span, "/=", Language::op_b);
  default: span.finish = loc.cursor; return NNT(span, "/", Language::op_b);
  }
}
} // namespace

NNT NextToken(SourceLocation &loc, error::Log *error_log) {
restart:
  // Delegate based on the next character in the file stream
  if (loc.source->seen_eof) {
    return NNT(loc.ToSpan(), "", Language::eof);
  } else if (IsAlphaOrUnderscore(*loc)) {
    return NextWord(loc);
  } else if (IsNonZeroDigit(*loc)) {
    return NextNumberInBase<10>(loc);
  }

  NNT nnt = NNT::Invalid();
  switch (*loc) {
  case '0': nnt = NextZeroInitiatedNumber(loc); break;
  case '`': nnt = NextCharLiteral(loc, error_log); break;
  case '"': nnt = NextStringLiteral(loc, error_log); break;
  case '/': nnt = NextSlashInitiatedToken(loc, error_log); break;
  case '\t':
  case ' ':
    loc.Increment();
    goto restart; // Skip whitespace
  case '\n':
  case '\0': loc.Increment(); return NNT(loc.ToSpan(), "", Language::newline);
  default: nnt = NextOperator(loc, error_log); break;
  }
  if (nnt == NNT::Invalid()) { goto restart; }
  return nnt;
}
