#ifndef ICARUS_UNITY
#include "Type/Type.h"
#endif

static inline bool IsLower(char c) { return ('a' <= c && c <= 'z'); }
static inline bool IsUpper(char c) { return ('A' <= c && c <= 'Z'); }
static inline bool IsNonZeroDigit(char c) { return ('1' <= c && c <= '9'); }
static inline bool IsDigit(char c){ return ('0' <= c && c <= '9'); }

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

#define RETURN_TERMINAL(term_type, ty, val)                                    \
  do {                                                                         \
    auto term_ptr           = new AST::Terminal;                               \
    term_ptr->loc           = cursor;                                          \
    term_ptr->terminal_type = Language::Terminal::term_type;                   \
    term_ptr->type          = ty;                                              \
    term_ptr->value = val;                                                     \
    return NNT(term_ptr, Language::expr);                                      \
  } while (false)

#define RETURN_NNT(tk, nt, tk_len)                                             \
  return NNT(new AST::TokenNode(Cursor::Behind(cursor, tk_len), tk),           \
             Language::nt)

extern std::map<const char *, Type *> PrimitiveTypes;

NNT NextWord(Cursor &cursor) {
  // Match [a-zA-Z_][a-zA-Z0-9_]*
  // We have already matched the first character

  auto starting_offset = cursor.offset;

  do { cursor.Increment(); } while (IsAlphaNumericOrUnderscore(*cursor));

  char old_char     = *cursor;
  *cursor           = '\0';
  std::string token = cursor.line.ptr + starting_offset;
  *cursor           = old_char;

  // Check if the word is a type primitive/literal and if so, build the
  // appropriate Node.
  for (const auto &type_lit : PrimitiveTypes) {
    if (type_lit.first == token) {
      RETURN_TERMINAL(Type, Type_, IR::Value::Type(type_lit.second));
    }
  }

  if (token == "true" || token == "false") {
    RETURN_TERMINAL(True, Bool, IR::Value::Bool(token == "true"));

  } else if (token == "null") {
    RETURN_TERMINAL(Null, NullPtr, IR::Value::None());

  } else if (token == "ord") {
    RETURN_TERMINAL(Ord, Func(Char, Uint), IR::Value::None());

  } else if (token == "ascii") {
    RETURN_TERMINAL(ASCII, Func(Uint, Char), IR::Value::None());

  } else if (token == "else") {
    auto term_ptr           = new AST::Terminal;
    Cursor loc              = cursor;
    loc.offset              = starting_offset;
    term_ptr->loc           = loc;
    term_ptr->terminal_type = Language::Terminal::Else;
    term_ptr->type          = Bool;

    return NNT(term_ptr, Language::kw_else);
  }

#define CASE_RETURN_NNT(str, op, len)                                          \
  do {                                                                         \
    if (strcmp(token_cstr, str) == 0) { RETURN_NNT(str, op, len); }            \
  } while (false)

  auto token_cstr = token.c_str();

  CASE_RETURN_NNT("in", op_b, 2);
  CASE_RETURN_NNT("print", op_l, 5);
  CASE_RETURN_NNT("import", op_l, 6);
  CASE_RETURN_NNT("free", op_l, 4);
  CASE_RETURN_NNT("while", kw_expr_block, 5);
  CASE_RETURN_NNT("for", kw_expr_block, 3);
  CASE_RETURN_NNT("if", kw_if, 2);
  CASE_RETURN_NNT("case", kw_block, 4);
  CASE_RETURN_NNT("enum", kw_block, 4);
  CASE_RETURN_NNT("struct", kw_struct, 6);
  CASE_RETURN_NNT("return", op_lt, 6);
  CASE_RETURN_NNT("continue", op_lt, 8);
  CASE_RETURN_NNT("break", op_lt, 5);
  CASE_RETURN_NNT("repeat", op_lt, 6);
  CASE_RETURN_NNT("restart", op_lt, 7);
  CASE_RETURN_NNT("scope", kw_struct, 5);

#undef CASE_RETURN_NNT

  Cursor loc = cursor;
  loc.offset = starting_offset;
  return NNT(new AST::Identifier(loc, token), Language::expr);
}

// Precondition: Output parameter points to a value of zero.
//
// Consumes longest sequence of alpha-numeric or underscore characters. Returns
// the number of digit characters read or -1. If the sequence consists only of
// '_'s or digits in the approprioate base, the output parameter points to the
// parsed value, and the number of digit characters read is returned. Otherwise,
// -1 is returned and there are no constraints on the value of the output
// parameter.
template<int Base>
static i32 ConsumeIntegerInBase(Cursor &cursor, i64 *val) {
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
    RETURN_TERMINAL(Int, Int, IR::Value::Int(0));
  }

  if (*cursor != '.') { RETURN_TERMINAL(Int, Int, IR::Value::Int(int_part)); }

  cursor.Increment();
  if (*cursor == '.') { // Looking at "..", not a fraction
    cursor.BackUp();
    RETURN_TERMINAL(Int, Int, IR::Value::Int(int_part));
  }

  i32 frac_digits = ConsumeIntegerInBase<Base>(cursor, &frac_part);
  if (frac_digits == -1) {
    // TODO log an error
    RETURN_TERMINAL(Real, Real, IR::Value::Real(0));
  }

  double val =
      int_part + (static_cast<double>(frac_part) / pow<Base>(frac_digits));
  RETURN_TERMINAL(Real, Real, IR::Value::Real(val));
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

  // TODO we assign to this repeatedly which is untenable perf-wise in the long-run.
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

  // Not leaked. It's owned by a terminal which is persistent.
  char *cstr = new char[str_lit.size() + 2];
  strcpy(cstr + 1, str_lit.c_str());
  cstr[0] = '\1';
  RETURN_TERMINAL(StringLiteral, String, IR::Value(cstr));
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
    RETURN_TERMINAL(Char, Char, IR::Value::Char('\0'));
  case '\\': {
    cursor.Increment();
    switch (*cursor) {
    case '\"': {
      result             = '"';
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
  RETURN_TERMINAL(Char, Char, IR::Value::Char(result));
}

static NNT NextOperator(Cursor &cursor) {
  switch (*cursor) {
  case '@': cursor.Increment(); RETURN_NNT("@", op_l, 1);
  case ',': cursor.Increment(); RETURN_NNT(",", comma, 1);
  case ';': cursor.Increment(); RETURN_NNT(";", semicolon, 1);
  case '(': cursor.Increment(); RETURN_NNT("(", l_paren, 1);
  case ')': cursor.Increment(); RETURN_NNT(")", r_paren, 1);
  case '[': cursor.Increment(); RETURN_NNT("[", l_bracket, 1);
  case ']': cursor.Increment(); RETURN_NNT("]", r_bracket, 1);
  case '$': cursor.Increment(); RETURN_NNT("$", op_l, 1);

  case '{': {
    cursor.Increment();
    if (*cursor =='{') {
      cursor.Increment();
      RETURN_NNT("{{", l_double_brace, 2);
    }
    RETURN_NNT("{", l_brace, 1);
  }
  case '}': {
    cursor.Increment();
    if (*cursor =='}') {
      cursor.Increment();
      RETURN_NNT("}}", r_double_brace, 2);
    }
    RETURN_NNT("}", r_brace, 1);
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
      RETURN_NNT(".", op_b, 1);
    } else {
      if (num_dots > 2) { ErrorLog::TooManyDots(cursor_copy, num_dots); }
      RETURN_NNT("..", dots, 2);
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
      RETURN_NNT("", newline, 0);
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

    char old_char       = *cursor;
    *cursor             = '\0';
    const char *tag_ref = cursor.line.ptr + cursor_copy.offset;
    size_t tag_len      = strlen(tag_ref);
    char *tag = new char[tag_len + 1];
    strcpy(tag, tag_ref);
    *cursor = old_char;

    RETURN_NNT(tag, hashtag, tag_len + 1);
  } break;

  case '+':
  case '%':
  case '<':
  case '>':
  case '|':
  case '^': {
    char first_char = *cursor;
    cursor.Increment();

    char *token = new char[3];
    token[0] = first_char;
    if (*cursor == '=') {
      cursor.Increment();
      token[1] = '=';
    } else {
      token[1] = '\0';
    }
    token[2] = '\0';

    RETURN_NNT(token, op_b, strlen(token));
  } break;

  case '*':
    cursor.Increment();
    if (*cursor == '/') {
      cursor.Increment();
      if (*cursor == '/') {
        // Looking at "*//" which should be parsed as an asterisk followed by a
        // one-line comment.
        cursor.BackUp();
        RETURN_NNT("*", op_b, 1);
      } else {
        ErrorLog::NotInMultilineComment(Cursor::Behind(cursor, 2));
        return NNT::Invalid();
      }
    } else if (*cursor == '=') {
      cursor.Increment();
      RETURN_NNT("*=", op_b, 2);
    } else {
      RETURN_NNT("*", op_b, 1);
    }

  case '&': {
    cursor.Increment();
    if (*cursor == '=') {
      cursor.Increment();
      RETURN_NNT("&=", op_b, 2);
    } else {
      RETURN_NNT("&", op_bl, 1);
    }
  } break;

  case ':': {
    cursor.Increment();

    if (*cursor == '=') {
      cursor.Increment();
      RETURN_NNT(":=", op_b, 2);

    } else if (*cursor == '>') {
      cursor.Increment();
      RETURN_NNT(":>", op_b, 2);

    } else {
      RETURN_NNT(":", colon, 1);
    }
  } break;

  case '!': {
    cursor.Increment();
    if (*cursor == '=') {
      cursor.Increment();
      RETURN_NNT("!=", op_b, 2);
    } else {
      RETURN_NNT("!", op_l, 1);
    }
  } break;

  case '-': {
    cursor.Increment();
    if (*cursor == '=') {
      cursor.Increment();
      RETURN_NNT("-=", op_b, 2);

    } else if (*cursor == '>') {
      cursor.Increment();
      auto nptr = new AST::TokenNode(cursor, "->");
      nptr->op = Language::Operator::Arrow;
      return NNT(nptr, Language::fn_arrow);

    } else if (*cursor == '-') {
      cursor.Increment();
      RETURN_TERMINAL(Hole, Unknown, IR::Value::None());

    } else {
      RETURN_NNT("-", op_bl, 1);
    }
  } break;

  case '=': {
    cursor.Increment();
    if (*cursor == '=') {
      cursor.Increment();
      RETURN_NNT("==", op_b, 2);

    } else if (*cursor == '>') {
      cursor.Increment();
      RETURN_NNT("=>", op_b, 2);

    } else {
      RETURN_NNT("=", eq, 1);
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
  case '_': UNREACHABLE;
  default: UNREACHABLE;
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
      if (cursor.source_file->ifs.eof()) {
        ErrorLog::RunawayMultilineComment();
        RETURN_NNT("", eof, 0);

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
  case '=': cursor.Increment(); RETURN_NNT("/=", op_b, 2);
  default: RETURN_NNT("/", op_b, 1);
  }
}

NNT NextToken(Cursor &cursor) {
restart:
  // Delegate based on the next character in the file stream
  if (cursor.source_file->ifs.eof()) {
    RETURN_NNT("", eof, 0);
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
  case ' ': cursor.Increment(); goto restart; // Skip whitespace
  case '\0': cursor.Increment(); RETURN_NNT("", newline, 0);
  default: nnt = NextOperator(cursor); break;
  }
  if (nnt == NNT::Invalid()) { goto restart; }
  return nnt;
}
#undef RETURN_NNT
#undef RETURN_TERMINAL
