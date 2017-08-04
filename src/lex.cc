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

static std::unordered_map<std::string, Type *> PrimitiveTypes{
#define PRIMITIVE_MACRO(GlobalName, EnumName, name) {#name, GlobalName},
#include "config/primitive.conf"
#undef PRIMITIVE_MACRO
};

NNT::NNT(const Cursor &cursor, const std::string &token, Language::NodeType nt)
    : node(std::make_unique<AST::TokenNode>(Cursor::Behind(cursor, token.size()), token)),
      node_type(nt) {}

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

NNT NextWord(Cursor &cursor) {
  // Match [a-zA-Z_][a-zA-Z0-9_]*
  // We have already matched the first character

  auto starting_offset = cursor.offset;

  do { cursor.Increment(); } while (IsAlphaNumericOrUnderscore(*cursor));

  std::string token =
      cursor.line.substr(starting_offset, cursor.offset - starting_offset);

  // Check if the word is a type primitive/literal and if so, build the
  // appropriate Node.
  for (const auto &type_lit : PrimitiveTypes) {
    if (type_lit.first == token) {
      return NNT::TerminalExpression(cursor, IR::Val::Type(type_lit.second));
    }
  }

  if (token == "true") {
    return NNT::TerminalExpression(cursor, IR::Val::Bool(true));

  } else if (token == "false") {
    return NNT::TerminalExpression(cursor, IR::Val::Bool(false));

  } else if (token == "null") {
    // Use Null(Void) to reprelent 'null' literal only
    return NNT::TerminalExpression(cursor, IR::Val::Null(Void));

  } else if (token == "ord") {
    return NNT::TerminalExpression(cursor, OrdFunc());

  } else if (token == "ascii") {
    return NNT::TerminalExpression(cursor, AsciiFunc());

  } else if (token == "error") {
    return NNT::TerminalExpression(cursor, ErrorFunc());
  }

  static const std::map<std::string, Language::NodeType> KeywordMap = {
      {"in", Language::op_b},           {"print", Language::op_l},
      {"require", Language::op_l},      {"free", Language::op_l},
      {"for", Language::kw_expr_block}, {"case", Language::kw_block},
      {"enum", Language::kw_block},     {"generate", Language::op_l},
      {"struct", Language::kw_struct},  {"return", Language::op_lt},
      {"continue", Language::op_lt},    {"break", Language::op_lt},
      {"repeat", Language::op_lt},      {"restart", Language::op_lt},
      {"scope", Language::kw_struct}};
  for (const auto &kv : KeywordMap) {
    if (token == kv.first) { return NNT(cursor, kv.first, kv.second); }
  }

  Cursor loc = cursor;
  loc.offset = starting_offset;
  return NNT(std::make_unique<AST::Identifier>(loc, token), Language::expr);
}

// Precondition: Output parameter points to a value of zero.
//
// Consumes longest sequence of alpha-numeric or underscore characters. Returns
// the number of digit characters read or -1. If the sequence consists only of
// '_'s or digits in the approprioate base, the output parameter points to the
// parsed value, and the number of digit characters read is returned. Otherwise,
// -1 is returned and there are no constraints on the value of the output
// parameter.
template <int Base> static i32 ConsumeIntegerInBase(Cursor &cursor, i64 *val) {
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
  int result = 1;
  for (int i = 0; i < num; ++i) { result *= Base; }
  return result;
}

template <int Base> static NNT NextNumberInBase(Cursor &cursor) {
  // TODO deal with bits_needed
  i64 int_part   = 0;
  i64 frac_part  = 0;
  i32 int_digits = ConsumeIntegerInBase<Base>(cursor, &int_part);
  if (int_digits == -1) {
    // TODO log an error
    // TODO Check for '.' and continue reading?
    return NNT::TerminalExpression(cursor, IR::Val::Int(0));
  }

  if (*cursor != '.') {
    return NNT::TerminalExpression(cursor, IR::Val::Int(int_part));
  }

  cursor.Increment();
  if (*cursor == '.') { // Looking at "..", not a fraction
    cursor.BackUp();
    return NNT::TerminalExpression(cursor, IR::Val::Int(int_part));
  }

  i32 frac_digits = ConsumeIntegerInBase<Base>(cursor, &frac_part);
  if (frac_digits == -1) {
    // TODO log an error
    return NNT::TerminalExpression(cursor, IR::Val::Real(0));
  }

  double val = static_cast<double>(int_part) +
               (static_cast<double>(frac_part) / pow<Base>(frac_digits));
  return NNT::TerminalExpression(cursor, IR::Val::Real(val));
}

static NNT NextZeroInitiatedNumber(Cursor &cursor) {
  cursor.Increment();

  switch (*cursor) {
  case 'b': cursor.Increment(); return NextNumberInBase<2>(cursor);
  case 'o': cursor.Increment(); return NextNumberInBase<8>(cursor);
  case 'd': cursor.Increment(); return NextNumberInBase<10>(cursor);
  case 'x': cursor.Increment(); return NextNumberInBase<16>(cursor);
  default: cursor.BackUp(); return NextNumberInBase<10>(cursor);
  }
}

static NNT NextStringLiteral(Cursor &cursor) {
  cursor.Increment();

  // TODO we assign to this repeatedly which is untenable perf-wise in the
  // long-run.
  std::string str_lit = "";

  for (; *cursor != '"' && *cursor != '\0'; cursor.Increment()) {
    if (*cursor != '\\') {
      str_lit += *cursor;
      continue;
    }

    cursor.Increment(); // Iterate past '\\'
    switch (*cursor) {
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
      if (*cursor == '\'') {
        ErrorLog::EscapedSingleQuoteInStringLit(Cursor::Behind(cursor, 1));
      } else {
        ErrorLog::InvalidEscapeCharInStringLit(Cursor::Behind(cursor, 1));
      }
      str_lit += *cursor;
      break;
    }
  }

  if (*cursor == '\0') {
    ErrorLog::RunawayStringLit(cursor);
  } else {
    cursor.Increment();
  }

  // TODO leaked? owned by terminal which should be persistent, so probably
  // okay.
  char *cstr = new char[str_lit.size() + 1];
  strcpy(cstr, str_lit.c_str());
  return NNT::TerminalExpression(cursor, IR::Val::StrLit(cstr));
}

static NNT NextCharLiteral(Cursor &cursor) {
  cursor.Increment();
  char result;

  switch (*cursor) {
  case '\t':
    ErrorLog::TabInCharLit(cursor);
    result = '\t';
    break;
  case ' ':
    // TODO error log
    result = '\0';
    break;
  case '\0':
    ErrorLog::RunawayCharLit(cursor);
    return NNT::TerminalExpression(cursor, IR::Val::Char('\0'));
  case '\\': {
    cursor.Increment();
    switch (*cursor) {
    case '\"': {
      result = '"';
      ErrorLog::EscapedDoubleQuoteInCharLit(Cursor::Behind(cursor, 1));
      cursor.Increment();
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
      ErrorLog::InvalidEscapeCharInCharLit(Cursor::Behind(cursor, 1));
      result = *cursor;
    }
    break;
  }
  default: { result = *cursor; } break;
  }
  cursor.Increment();
  return NNT::TerminalExpression(cursor, IR::Val::Char(result));
}

static NNT NextOperator(Cursor &cursor) {
  switch (*cursor) {
  case '@': cursor.Increment(); return NNT(cursor, "@", Language::op_l);
  case ',': cursor.Increment(); return NNT(cursor, ",", Language::comma);
  case ';': cursor.Increment(); return NNT(cursor, ";", Language::semicolon);
  case '(': cursor.Increment(); return NNT(cursor, "(", Language::l_paren);
  case ')': cursor.Increment(); return NNT(cursor, ")", Language::r_paren);
  case '[': cursor.Increment(); return NNT(cursor, "[", Language::l_bracket);
  case ']': cursor.Increment(); return NNT(cursor, "]", Language::r_bracket);
  case '$': cursor.Increment(); return NNT(cursor, "$", Language::op_l);

  case '{': {
    cursor.Increment();
    if (*cursor == '{') {
      cursor.Increment();
      return NNT(cursor, "{{", Language::l_double_brace);
    }
    return NNT(cursor, "{", Language::l_brace);
  }
  case '}': {
    cursor.Increment();
    if (*cursor == '}') {
      cursor.Increment();
      return NNT(cursor, "}}", Language::r_double_brace);
    }
    return NNT(cursor, "}", Language::r_brace);
  }

  case '.': {
    Cursor cursor_copy = cursor;

    while (*cursor == '.') { cursor.Increment(); }
    size_t num_dots = cursor.offset - cursor_copy.offset;

    if (num_dots == 1) {
      if (IsDigit(*cursor)) {
        cursor.BackUp();
        return NextNumberInBase<10>(cursor);
      }
      return NNT(cursor, ".", Language::op_b);
    } else {
      if (num_dots > 2) { ErrorLog::TooManyDots(cursor_copy, num_dots); }
      return NNT(cursor, "..", Language::dots);
    }
  } break;

  case '\\': {
    Cursor cursor_copy = cursor;
    size_t dist        = 1;

    cursor.Increment();
    ++dist;
    switch (*cursor) {
    case '\\':
      cursor.Increment();
      return NNT(cursor, "", Language::newline);
      break;
    case '(':
      cursor.Increment();
      return NNT(cursor, "\\(", Language::l_ref);
      break;
    case '\0':
      // Ignore the following newline and retry
      cursor.Increment();
      return NNT::Invalid();
    case ' ':
    case '\t':
      while (IsWhitespace(*cursor)) {
        cursor.Increment();
        ++dist;
      }
      if (*cursor == '\0') {
        cursor.Increment();
        return NNT::Invalid();
      }

    // Intentionally falling through. Looking at a non-whitespace after a '\'
    default:
      ErrorLog::NonWhitespaceAfterNewlineEscape(cursor_copy, dist);
      return NNT::Invalid();
    }
  } break;

  case '#': {
    NOT_YET;
    /*
    cursor.Increment();
    Cursor cursor_copy = cursor;

    if (!IsAlpha(*cursor)) {
      ErrorLog::InvalidHashtag(cursor_copy);
      return NNT::Invalid();
    }

    do { cursor.Increment(); } while (IsAlphaNumericOrUnderscore(*cursor));

    if (cursor.offset - cursor_copy.offset == 0) {
      ErrorLog::InvalidHashtag(cursor_copy);
    }

    std::string tag = cursor.line.substr(cursor_copy.offset,
                                         cursor.offset - cursor_copy.offset);
    return NNT(cursor, tag, Language::hashtag);
    */
  } break;

  case '+':
  case '%':
  case '<':
  case '>':
  case '|':
  case '^': {
    char first_char = *cursor;
    cursor.Increment();

    std::string token = "X=";
    token[0]          = first_char;
    if (*cursor == '=') {
      cursor.Increment();
    } else {
      token = std::string(1, first_char);
    }

    return NNT(cursor, token, Language::op_b);
  } break;

  case '*':
    cursor.Increment();
    if (*cursor == '/') {
      cursor.Increment();
      if (*cursor == '/') {
        // Looking at "*//" which should be parsed as an asterisk followed by a
        // one-line comment.
        cursor.BackUp();
    return NNT(cursor, "*", Language::op_b);
      } else {
        ErrorLog::NotInMultilineComment(Cursor::Behind(cursor, 2));
        return NNT::Invalid();
      }
    } else if (*cursor == '=') {
      cursor.Increment();
      return NNT(cursor, "*=", Language::op_b);
    } else {
      return NNT(cursor, "*", Language::op_bl);
    }

  case '&': {
    cursor.Increment();
    if (*cursor == '=') {
      cursor.Increment();
      return NNT(cursor, "&=", Language::op_b);
    } else {
      return NNT(cursor, "&", Language::op_bl);
    }
  } break;

  case ':': {
    cursor.Increment();

    if (*cursor == '=') {
      cursor.Increment();
      return NNT(cursor, ":=", Language::op_b);

    } else {
      return NNT(cursor, ":", Language::colon);
    }
  } break;

  case '!': {
    cursor.Increment();
    if (*cursor == '=') {
      cursor.Increment();
      return NNT(cursor, "!=", Language::op_b);
    } else {
      return NNT(cursor, "!", Language::op_l);
    }
  } break;

  case '-': {
    cursor.Increment();
    if (*cursor == '=') {
      cursor.Increment();
      return NNT(cursor, "-=", Language::op_b);

    } else if (*cursor == '>') {
      cursor.Increment();
      auto nptr = std::make_unique<AST::TokenNode>(cursor, "->");
      nptr->op  = Language::Operator::Arrow;
      return NNT(std::move(nptr), Language::fn_arrow);

    } else if (*cursor == '-') {
      cursor.Increment();
      return NNT::TerminalExpression(cursor, IR::Val::None());

    } else {
      return NNT(cursor, "-", Language::op_bl);
    }
  } break;

  case '=': {
    cursor.Increment();
    if (*cursor == '=') {
      cursor.Increment();
      return NNT(cursor, "==", Language::op_b);

    } else if (*cursor == '>') {
      cursor.Increment();
      return NNT(cursor, "=>", Language::op_b);

    } else {
      return NNT(cursor, "=", Language::eq);
    }
  } break;
  case '?':
    ErrorLog::InvalidCharQuestionMark(cursor);
    cursor.Increment();
    return NNT::Invalid();
  case '~':
    ErrorLog::InvalidCharTilde(cursor);
    cursor.Increment();
    return NNT::Invalid();
  case '\'': cursor.Increment(); return NNT(cursor, "'", Language::op_bl);
  case '_': UNREACHABLE;
  default:
    std::cerr << "----> " << static_cast<int>(*cursor) << std::endl;
    UNREACHABLE;
  }
}

NNT NextSlashInitiatedToken(Cursor &cursor) {
  cursor.Increment();
  switch (*cursor) {
  case '/': // line comment
    cursor.SkipToEndOfLine();
    return NNT::Invalid();
  case '*': { // Multiline comment
    cursor.Increment();
    char back_one = *cursor;
    cursor.Increment();

    u64 comment_layer = 1;
    while (comment_layer != 0) {
      if (cursor.seen_eof_) {
        ErrorLog::RunawayMultilineComment();
        return NNT(cursor, "", Language::eof);

      } else if (back_one == '/' && *cursor == '*') {
        ++comment_layer;

      } else if (back_one == '*' && *cursor == '/') {
        --comment_layer;
      }

      back_one = *cursor;
      cursor.Increment();
    }
    return NNT::Invalid();
  }
  case '=': cursor.Increment(); return NNT(cursor, "/=", Language::op_b);
  default: return NNT(cursor, "/", Language::op_b);
  }
}

NNT NextToken(Cursor &cursor) {
restart:
  // Delegate based on the next character in the file stream
  if (cursor.seen_eof_) {
    return NNT(cursor, "", Language::eof);
  } else if (IsAlphaOrUnderscore(*cursor)) {
    return NextWord(cursor);
  } else if (IsNonZeroDigit(*cursor)) {
    return NextNumberInBase<10>(cursor);
  }

  NNT nnt = NNT::Invalid();
  switch (*cursor) {
  case '0': nnt = NextZeroInitiatedNumber(cursor); break;
  case '`': nnt = NextCharLiteral(cursor); break;
  case '"': nnt = NextStringLiteral(cursor); break;
  case '/': nnt = NextSlashInitiatedToken(cursor); break;
  case '\t':
  case ' ':
    cursor.Increment();
    goto restart; // Skip whitespace
  case '\n':
  case '\0': cursor.Increment(); return NNT(cursor, "", Language::newline);
  default: nnt = NextOperator(cursor); break;
  }
  if (nnt == NNT::Invalid()) { goto restart; }
  return nnt;
}
