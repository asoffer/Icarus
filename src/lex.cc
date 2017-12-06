#include "ast/ast.h"
#include "error_log.h"
#include "nnt.h"
#include "type/type.h"
#include <cstring>
#include <unordered_map>

#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  Type *GlobalName = new Primitive(PrimType::EnumName);
#include "config/primitive.conf"
#undef PRIMITIVE_MACRO

NNT::NNT(const TextSpan &span, const std::string &token, Language::NodeType nt)
    : node(std::make_unique<AST::TokenNode>(span, token)), node_type(nt) {}

extern IR::Val ErrorFunc();
extern IR::Val AsciiFunc();
extern IR::Val OrdFunc();

static inline bool IsLower(char c) { return ('a' <= c && c <= 'z'); }
static inline bool IsUpper(char c) { return ('A' <= c && c <= 'Z'); }
static inline bool IsNonZeroDigit(char c) { return ('1' <= c && c <= '9'); }
static inline bool IsDigit(char c) { return ('0' <= c && c <= '9'); }

template <int Base> static inline i32 DigitInBase(char c);
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

static inline bool IsAlpha(char c) { return IsLower(c) || IsUpper(c); }
static inline bool IsAlphaNumeric(char c) { return IsAlpha(c) || IsDigit(c); }
static inline bool IsWhitespace(char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}
static inline bool IsAlphaOrUnderscore(char c) {
  return IsAlpha(c) || (c == '_');
}
static inline bool IsAlphaNumericOrUnderscore(char c) {
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
  {#name, IR::Val::Type(GlobalName)},
#include "config/primitive.conf"
#undef PRIMITIVE_MACRO
      {"true", IR::Val::Bool(true)}, {"false", IR::Val::Bool(false)},
      {"null", IR::Val::NullPtr()},  {"ord", OrdFunc()},
      {"ascii", AsciiFunc()},        {"error", ErrorFunc()},
  };
  auto iter = Reserved.find(token);
  if (iter != Reserved.end()) {
    return NNT::TerminalExpression(span, iter->second);
  }

  static const std::map<std::string, Language::NodeType> KeywordMap = {
      {"in", Language::op_b},           {"print", Language::op_l},
      {"ensure", Language::op_l},       {"needs", Language::op_l},
      {"require", Language::op_l},      {"free", Language::op_l},
      {"for", Language::kw_expr_block}, {"case", Language::kw_block},
      {"enum", Language::kw_block},     {"generate", Language::op_l},
      {"struct", Language::kw_struct},  {"return", Language::op_lt},
      {"continue", Language::op_lt},    {"break", Language::op_lt},
      {"repeat", Language::op_lt},      {"restart", Language::op_lt},
      {"scope", Language::kw_struct}};
  for (const auto &kv : KeywordMap) {
    if (token == kv.first) { return NNT(span, kv.first, kv.second); }
  }

  return NNT(std::make_unique<AST::Identifier>(span, token), Language::expr);
}

// Precondition: Output parameter points to a value of zero.
//
// Consumes longest sequence of alpha-numeric or underscore characters. Returns
// the number of digit characters read or -1. If the sequence consists only of
// '_'s or digits in the approprioate base, the output parameter points to the
// parsed value, and the number of digit characters read is returned. Otherwise,
// -1 is returned and there are no constraints on the value of the output
// parameter.
template <int Base> static i32 ConsumeIntegerInBase(SourceLocation &cursor, i32 *val) {
  i32 chars_read = 0;
start:
  if (*cursor == '_') {
    cursor.Increment();
    goto start;
  }

  int digit = DigitInBase<Base>(*cursor);
  if (digit != -1) {
    *val = (*val * Base) + digit;
    ++chars_read;
    cursor.Increment();
    goto start;
  }

  if (IsAlphaNumeric(*cursor)) {
    while (IsAlphaNumeric(*cursor)) { cursor.Increment(); }
    return -1;
  }

  return chars_read;
}

template <int Base> static inline int pow(i32 num) {
  // TODO repeated squaring
  int result = 1;
  for (int i = 0; i < num; ++i) { result *= Base; }
  return result;
}

template <int Base> static NNT NextNumberInBase(SourceLocation &loc) {
  auto span = loc.ToSpan();
  // TODO deal with bits_needed
  i32 int_part   = 0;
  i32 frac_part  = 0;
  i32 int_digits = ConsumeIntegerInBase<Base>(loc, &int_part);
  span.finish = loc.cursor;

  if (int_digits == -1) {
    // TODO log an error
    // TODO Check for '.' and continue reading?
    return NNT::TerminalExpression(span, IR::Val::Int(0));
  }

  if (*loc!= '.') {
    return NNT::TerminalExpression(span, IR::Val::Int(int_part));
  }

  loc.Increment();
  if (*loc== '.') { // Looking at "..", not a fraction
    loc.BackUp();
    span.finish = loc.cursor;
    return NNT::TerminalExpression(span, IR::Val::Int(int_part));
  }

  i32 frac_digits = ConsumeIntegerInBase<Base>(loc, &frac_part);
  span.finish = loc.cursor;
  if (frac_digits == -1) {
    // TODO log an error
    return NNT::TerminalExpression(span, IR::Val::Real(0));
  }

  double val = static_cast<double>(int_part) +
               (static_cast<double>(frac_part) / pow<Base>(frac_digits));
  return NNT::TerminalExpression(span, IR::Val::Real(val));
}

static NNT NextZeroInitiatedNumber(SourceLocation &loc) {
  loc.Increment();

  switch (*loc) {
  case 'b': loc.Increment(); return NextNumberInBase<2>(loc);
  case 'o': loc.Increment(); return NextNumberInBase<8>(loc);
  case 'd': loc.Increment(); return NextNumberInBase<10>(loc);
  case 'x': loc.Increment(); return NextNumberInBase<16>(loc);
  default: loc.BackUp(); return NextNumberInBase<10>(loc);
  }
}

static NNT NextStringLiteral(SourceLocation &loc) {
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
      if (*loc == '\'') {
        ErrorLog::EscapedSingleQuoteInStringLit(span);
      } else {
        ErrorLog::InvalidEscapeCharInStringLit(span);
      }
      str_lit += *loc;
      break;
    }
  }

  if (*loc == '\n') {
    ErrorLog::RunawayStringLit(span);
  } else {
    loc.Increment();
  }

  span.finish = loc.cursor;
  return NNT::TerminalExpression(span, IR::Val::StrLit(str_lit));
}

static NNT NextCharLiteral(SourceLocation &loc) {
  auto span = loc.ToSpan();
  loc.Increment();
  span.finish = loc.cursor;
  char result;

  switch (*loc) {
  case '\t':
    ErrorLog::TabInCharLit(span);
    result = '\t';
    break;
  case ' ':
    // TODO error log
    result = '\0';
    break;
  case '\0':
    span.finish = loc.cursor;
    ErrorLog::RunawayCharLit(span);
    return NNT::TerminalExpression(span, IR::Val::Char('\0'));
  case '\\': {
    loc.Increment();
    switch (*loc) {
    case '\"': {
      result = '"';
      loc.Increment();
      span.finish = loc.cursor;
      ErrorLog::EscapedDoubleQuoteInCharLit(span);
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
      ErrorLog::InvalidEscapeCharInCharLit(span);
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

static NNT NextOperator(SourceLocation &loc) {
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
    span.finish = loc.cursor;
    size_t num_dots = span.finish.offset - span.start.offset;

    if (num_dots == 1) {
      if (IsDigit(*loc)) {
        loc.BackUp();
        return NextNumberInBase<10>(loc);
      }
      span.finish = loc.cursor;
      return NNT(span, ".", Language::op_b);
    } else {
      if (num_dots > 2) { ErrorLog::TooManyDots(span, num_dots);
      }
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

    // Intentionally falling through. Looking at a non-whitespace after a '\'
    default:
      span.finish = loc.cursor;
      ErrorLog::NonWhitespaceAfterNewlineEscape(span, dist);
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
       ErrorLog::NotInMultilineComment(span);
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
      auto nptr = std::make_unique<AST::TokenNode>(span, "->");
      nptr->op  = Language::Operator::Arrow;
      return NNT(std::move(nptr), Language::fn_arrow);

    } else if (*loc == '-') {
      loc.Increment();
      span.finish = loc.cursor;
      return NNT::TerminalExpression(span, IR::Val::None());

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
    ErrorLog::InvalidCharQuestionMark(span);
    return NNT::Invalid();
  case '~':
    loc.Increment();
    span.finish = loc.cursor;
    ErrorLog::InvalidCharTilde(span);
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

NNT NextSlashInitiatedToken(SourceLocation &loc) {
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
        ErrorLog::RunawayMultilineComment();
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

NNT NextToken(SourceLocation &loc) {
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
  case '`': nnt = NextCharLiteral(loc); break;
  case '"': nnt = NextStringLiteral(loc); break;
  case '/': nnt = NextSlashInitiatedToken(loc); break;
  case '\t':
  case ' ':
    loc.Increment();
    goto restart; // Skip whitespace
  case '\n':
  case '\0': loc.Increment(); return NNT(loc.ToSpan(), "", Language::newline);
  default: nnt = NextOperator(loc); break;
  }
  if (nnt == NNT::Invalid()) { goto restart; }
  return nnt;
}
