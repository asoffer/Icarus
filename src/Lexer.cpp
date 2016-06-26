#include "Lexer.h"

#ifndef ICARUS_UNITY
#include "Type/Type.h"
#endif

extern std::queue<std::string> file_queue;

static inline bool IsLower(char c) { return ('a' <= c && c <= 'z'); }
static inline bool IsUpper(char c) { return ('A' <= c && c <= 'Z'); }
static inline bool IsDigit(char c) { return ('0' <= c && c <= '9'); }
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
// static inline bool IsNewline(char c) { return (char)10 <= c && c <= (char)13; }
static inline bool IsNonGraphic(char c) {
  return c < (char)9 || ((char)13 < c && c < (char)32) || c == (char)127;
}

#define RETURN_TERMINAL(term_type, ty, val)                                    \
  auto term_ptr           = new AST::Terminal;                                 \
  term_ptr->loc           = cursor.Location();                                 \
  term_ptr->terminal_type = Language::Terminal::term_type;                     \
  term_ptr->type          = ty;                                                \
  term_ptr->value = val;                                                       \
  return NNT(term_ptr, Language::expr);

#define RETURN_NNT(tk, nt)                                                     \
  return NNT(new AST::TokenNode(cursor.Location(), tk), Language::nt);

namespace TypeSystem {
extern std::map<std::string, Type *> Literals;
} // namespace TypeSystem

// Take a filename as a string or a C-string and opens the named file
Lexer::Lexer(SourceFile *sf) : source_file_(sf) {
  char *file_name = new char[source_file_->name.size() + 1];
  std::strcpy(file_name, source_file_->name.c_str());
  cursor.file_name_ = file_name;

  ifs = std::ifstream(file_name, std::ifstream::in);

  pstr temp_blank;
  source_file_->lines.push_back(temp_blank); // Blank line since we 1-index.

  MoveCursorToNextLine();
}

Lexer::~Lexer() { ifs.close(); }

// Copy in the next line of text from the file, and scrub away non-graphic
// characters (replacing them with ' '). We leave characters in the range 32-126
// as well as 9-13.
void Lexer::MoveCursorToNextLine() {
  assert(!ifs.eof());
  std::string temp;
  std::getline(ifs, temp);

  // Check for null characters in line
  size_t line_length = temp.size();
  for (size_t i = 0; i < line_length; ++i) {
    if (temp[i] == '\0') {
      temp[i] = ' ';
      Error::Log::Log(Error::MsgId::NullCharInSource, cursor.Location(), 0, 0);
    } else if (IsNonGraphic(temp[i])) {
      temp[i] = ' ';
      Error::Log::Log(Error::MsgId::NonGraphicCharInSource, cursor.Location(), 0, 0);
    }
  }

  cursor.offset_ = 0;
  cursor.line_   = pstr(temp.c_str());

  ++cursor.line_num_;
  source_file_->lines.push_back(cursor.line_);
}

void Lexer::IncrementCursor() {
  if (*cursor != '\0') {
    ++cursor.offset_;
  } else {
    MoveCursorToNextLine();
  }
}

void Lexer::BackUpCursor() {
  // You can't back up to a previous line.
  assert(cursor.offset_ > 0);
  --cursor.offset_;
}

void Lexer::SkipToEndOfLine() {
  while (*cursor != '\0') { ++cursor.offset_; }
}

// Get the next token
NNT Lexer::Next() {
restart:
  // Delegate based on the next character in the file stream
  if (ifs.eof()) {
    RETURN_NNT("", eof);

  } else if (IsAlphaOrUnderscore(*cursor)) {
    return NextWord();

  } else if (IsDigit(*cursor)) {
    return NextNumber();
  }

  switch (*cursor) {
  case '\t':
  case ' ': IncrementCursor(); goto restart; // Explicit TCO
  case '\0': IncrementCursor(); RETURN_NNT("", newline);
  default: return NextOperator();
  }
}

NNT Lexer::NextWord() {
  // Match [a-zA-Z_][a-zA-Z0-9_]*
  // We have already matched the first character

  auto starting_offset = cursor.offset_;

  do {
    IncrementCursor();
  } while (IsAlphaNumericOrUnderscore(*cursor));

  char old_char     = *cursor;
  *cursor           = '\0';
  std::string token = cursor.line_.ptr + starting_offset;
  *cursor           = old_char;

  // Check if the word is a type primitive/literal and if so, build the
  // appropriate Node.
  for (const auto &type_lit : TypeSystem::Literals) {
    if (type_lit.first == token) {
      RETURN_TERMINAL(Type, Type_, Context::Value(type_lit.second));
    }
  }

  if (token == "true") {
    RETURN_TERMINAL(True, Bool, Context::Value(true));

  } else if (token == "false") {
    RETURN_TERMINAL(False, Bool, Context::Value(false));

  } else if (token == "null") {
    RETURN_TERMINAL(Null, NullPtr, nullptr);

  } else if (token == "ord") {
    // NOTE: Cannot use Context::Value(builtin::ord()) because it hasn't been
    // initialized yet.
    RETURN_TERMINAL(Ord, Func(Char, Uint), nullptr);

  } else if (token == "ascii") {
    // NOTE: Cannot use Context::Value(builtin::ascii()) because it hasn't been
    // initialized yet.
    RETURN_TERMINAL(ASCII, Func(Uint, Char), nullptr);

  } else if (token == "else") {
    auto term_ptr           = new AST::Terminal;
    term_ptr->loc           = cursor.Location();
    term_ptr->terminal_type = Language::Terminal::Else;
    term_ptr->type          = Bool;

    return NNT(term_ptr, Language::kw_else);

  } else if (token == "in") {
    RETURN_NNT("in", op_b);

  } else if (token == "print" || token == "import" || token == "free") {
    RETURN_NNT(token, op_l);

  } else if (token == "while" || token == "for") {
    RETURN_NNT(token, kw_expr_block);

  } else if (token == "if") {
    RETURN_NNT(token, kw_if);

  } else if (token == "case" || token == "enum") {
    RETURN_NNT(token, kw_block);

  } else if (token == "struct") {
    RETURN_NNT(token, kw_struct);

  } else if (token == "return" || token == "continue" || token == "break" ||
             token == "repeat" || token == "restart") {
    RETURN_NNT(token, op_lt);
  }

  if (token == "string") { file_queue.emplace("lib/string.ic"); }
  return NNT(new AST::Identifier(cursor.Location(), token), Language::expr);
}

NNT Lexer::NextNumber() {
  auto starting_offset = cursor.offset_;

  do { IncrementCursor(); } while (IsDigit(*cursor));

  switch (*cursor) {
  case 'u':
  case 'U': {
    IncrementCursor();

    char old_char = *cursor;
    *cursor       = '\0';
    auto uint_val = std::stoul(cursor.line_.ptr + starting_offset);
    *cursor       = old_char;

    RETURN_TERMINAL(Uint, Uint, Context::Value(uint_val));
  } break;

  case '.': {
    IncrementCursor();
    if (*cursor == '.') {
      BackUpCursor();
      goto return_int_label;
    }

    // Just one dot. Should be interpretted as a decimal point.
    IncrementCursor();
    while (IsDigit(*cursor)) { IncrementCursor(); }

    char old_char = *cursor;
    *cursor       = '\0';
    auto real_val = std::stod(cursor.line_.ptr + starting_offset);
    *cursor       = old_char;

    RETURN_TERMINAL(Real, Real, Context::Value(real_val));
  } break;

  default:
  return_int_label : {
    char old_char = *cursor;
    *cursor       = '\0';
    auto int_val = std::stol(cursor.line_.ptr + starting_offset);
    *cursor       = old_char;
    RETURN_TERMINAL(Int, Int, Context::Value(int_val));
  } break;
  }
}

NNT Lexer::NextOperator() {
  switch (*cursor) {
  case '`': IncrementCursor(); RETURN_NNT("`", op_bl);
  case '@': IncrementCursor(); RETURN_NNT("@", op_l);
  case ',': IncrementCursor(); RETURN_NNT(",", comma);
  case ';': IncrementCursor(); RETURN_NNT(";", semicolon);
  case '(': IncrementCursor(); RETURN_NNT("(", l_paren);
  case ')': IncrementCursor(); RETURN_NNT(")", r_paren);
  case '[': IncrementCursor(); RETURN_NNT("[", l_bracket);
  case ']': IncrementCursor(); RETURN_NNT("]", r_bracket);
  case '{': IncrementCursor(); RETURN_NNT("{", l_brace);
  case '}': IncrementCursor(); RETURN_NNT("}", r_brace);
  case '$': IncrementCursor(); RETURN_NNT("$", op_l);

  case '.': {
    TokenLocation loc   = cursor.Location();
    size_t saved_offset = cursor.offset_;
    // Note: safe because we know we have a null-terminator
    while (*cursor == '.') { IncrementCursor(); }
    size_t num_dots = cursor.offset_ - saved_offset;

    if (num_dots == 1) {
      RETURN_NNT(".", op_b);
    } else {
      if (num_dots > 2) {
        Error::Log::Log(Error::MsgId::TooManyDots, loc, 0, num_dots);
      }
      RETURN_NNT("..", dots);
    }
  } break;

  case '\\': {
    TokenLocation loc = cursor.Location();
    size_t dist = 1;

    IncrementCursor();
    ++dist;
    switch(*cursor) {
    case '\\':
      IncrementCursor();
      RETURN_NNT("", newline);
      break;
    case '\0':
      // Ignore the following newline
      IncrementCursor();
      return Next();
    case ' ':
    case '\t':
      while (IsWhitespace(*cursor)) {
        IncrementCursor();
        ++dist;
      }
      if (*cursor == '\0') {
        IncrementCursor();
        return Next();
      }

    // Intentionally falling through. Looking at a non-whitespace after a '\'
    default:
      Error::Log::Log(Error::MsgId::NonWhitespaceAfterNewlineEscape, loc, 0,
                      dist);
      return Next();
    }
  } break;

  case '#': {
    IncrementCursor();
    TokenLocation loc    = cursor.Location();
    auto starting_offset = cursor.offset_;

    if (!IsAlpha(*cursor)) {
      Error::Log::Log(Error::MsgId::InvalidHashtag, loc, 0, 1);
      return Next();
    }

    do { IncrementCursor(); } while (IsAlphaNumericOrUnderscore(*cursor));

    if (cursor.offset_ - starting_offset == 0) {
      Error::Log::Log(Error::MsgId::InvalidHashtag, loc, 0, 1);
    }

    char old_char   = *cursor;
    *cursor         = '\0';
    std::string tag = cursor.line_.ptr + starting_offset;
    *cursor         = old_char;

    RETURN_NNT(tag, hashtag);
  } break;

  case '+':
  case '%':
  case '<': 
  case '>': 
  case '|':
  case '^': {
    char first_char = *cursor;
    IncrementCursor();
    std::string token(*cursor == '=' ? 2 : 1, first_char);
    if (*cursor == '=') {
      IncrementCursor();
      token[1] = '=';
    }
    RETURN_NNT(token, op_b);
  } break;

  case '*':
    IncrementCursor();
    if (*cursor == '/') {
      IncrementCursor();
      if (*cursor == '/') {
        // Looking at "*//" which should be parsed as an asterisk followed by a
        // one-line comment.
        BackUpCursor();
        RETURN_NNT("*", op_b);
      } else {
        TokenLocation loc = cursor.Location();
        loc.offset -= 2;
        Error::Log::Log(Error::MsgId::NotInMultilineComment, loc, 0, 2);
        return Next();
      }
    } else if (*cursor == '=') {
      IncrementCursor();
      RETURN_NNT("*=", op_b);
    } else {
      RETURN_NNT("*", op_b);
    }

  case '&': {
    IncrementCursor();
    if (*cursor == '=') {
      IncrementCursor();
      RETURN_NNT("&=", op_b);
    } else {
      RETURN_NNT("&", op_bl);
    }
  } break;

  case ':':  {
    IncrementCursor();

    if (*cursor == '=') {
      IncrementCursor();
      RETURN_NNT(":=", op_b);

    } else if (*cursor == '>') {
      IncrementCursor();
      RETURN_NNT(":>", op_b);

    } else {
      RETURN_NNT(":", colon);
    }
  } break;

  case '!': {
    IncrementCursor();
    if (*cursor == '=') {
      IncrementCursor();
      RETURN_NNT("!=", op_b);
    } else {
      RETURN_NNT("!", op_l);
    }
  } break;

  case '-': {
    IncrementCursor();
    if (*cursor == '=') {
      IncrementCursor();
      RETURN_NNT("-=", op_b);

    } else if (*cursor == '>') {
      IncrementCursor();
      auto nptr = new AST::TokenNode(cursor.Location(), "->");
      nptr->op = Language::Operator::Arrow;
      return NNT(nptr, Language::fn_arrow);

    } else if (*cursor == '-') {
      IncrementCursor();
      RETURN_TERMINAL(Hole, Unknown, nullptr);

    } else {
      RETURN_NNT("-", op_bl);
    }
  } break;

  case '=': {
    IncrementCursor();
    if (*cursor == '=') {
      IncrementCursor();
      RETURN_NNT("==", op_b);

    } else if (*cursor == '>') {
      IncrementCursor();
      RETURN_NNT("=>", op_b);

    } else {
      RETURN_NNT("=", eq);
    }
  } break;

  case '/': {
    IncrementCursor();
    if (*cursor == '/') {
      // Ignore comments altogether
      SkipToEndOfLine();
      return Next();

    } else if (*cursor == '=') {
      IncrementCursor();
      RETURN_NNT("/=", op_b);

    } else if (*cursor == '*') {
      IncrementCursor();
      char back_one = *cursor;
      IncrementCursor();

      size_t comment_layer = 1;

      while (comment_layer != 0) {
        if (ifs.eof()) {
          Error::Log::Log(Error::MsgId::RunawayMultilineComment,
                          cursor.Location(), 0, 2);
          RETURN_NNT("", eof);

        } else if (back_one == '/' && *cursor == '*') {
          ++comment_layer;

        } else if (back_one == '*' && *cursor == '/') {
          --comment_layer;
        }

        back_one = *cursor;
        IncrementCursor();
      }

      // Ignore comments altogether
      return Next();

    } else {
      RETURN_NNT("/", op_b);
    }

  } break;

  case '"': {
    IncrementCursor();
    file_queue.emplace("lib/string.ic");

    std::string str_lit = "";

    while (*cursor != '"' && *cursor != '\0') {
      if (*cursor == '\\') {
        IncrementCursor();
        switch (*cursor) {
        case '\'': {
          str_lit += '\'';
          TokenLocation loc = cursor.Location();
          --loc.offset;
          Error::Log::Log(Error::MsgId::EscapedSingleQuoteInStringLit, loc, 0,
                          2);
        } break;

        case '\\': str_lit += '\\'; break;
        case '"':  str_lit += '"';  break;
        case 'a':  str_lit += '\a'; break;
        case 'b':  str_lit += '\b'; break;
        case 'f':  str_lit += '\f'; break;
        case 'n':  str_lit += '\n'; break;
        case 'r':  str_lit += '\r'; break;
        case 't':  str_lit += '\t'; break;
        case 'v':  str_lit += '\v'; break;

        default: {
          TokenLocation loc = cursor.Location();
          --loc.offset;
          Error::Log::Log(Error::MsgId::InvalidEscapeCharInStringLit, loc, 0,
                          2);

            str_lit += *cursor;
          } break;
        }
      } else {
        str_lit += *cursor;
      }

      IncrementCursor();
    }

    if (*cursor == '\0') {
      Error::Log::Log(Error::MsgId::RunawayStringLit, cursor.Location(), 0, 1);
    } else {
      IncrementCursor();
    }

    // Not leaked. It's owned by a terminal which is persistent.
    char *cstr = new char[str_lit.size() + 1];
    std::strcpy(cstr, str_lit.c_str());
    RETURN_TERMINAL(StringLiteral, Unknown, Context::Value(cstr));
  } break;

  case '\'': {
    IncrementCursor();
    char result;

    switch (*cursor) {
    case '\t':
      Error::Log::Log(Error::MsgId::TabInCharLit, cursor.Location(), 0, 1);
      result = '\t';
      break;

    case '\0': {
      Error::Log::Log(Error::MsgId::RunawayCharLit, cursor.Location(), 0, 1);

      RETURN_TERMINAL(Char, Char, Context::Value('\0'));
    }
    case '\\': {
      IncrementCursor();
      switch (*cursor) {
      case '\"': {
        result = '"';
        TokenLocation loc = cursor.Location();
        --loc.offset;
        Error::Log::Log(Error::MsgId::EscapedDoubleQuoteInCharLit, loc, 0, 2);
      } break;
      case '\\': result = '\\'; break;
      case '\'': result = '\''; break;
      case 'a': result  = '\a'; break;
      case 'b': result  = '\b'; break;
      case 'f': result  = '\f'; break;
      case 'n': result  = '\n'; break;
      case 'r': result  = '\r'; break;
      case 't': result  = '\t'; break;
      case 'v': result  = '\v'; break;
      default:
        TokenLocation loc = cursor.Location();
        --loc.offset;
        Error::Log::Log(Error::MsgId::InvalidEscapeCharInCharLit, loc, 0, 2);
        result = *cursor;
      }
      break;
    }
    default: { result = *cursor; } break;
    }

    IncrementCursor();

    if (*cursor == '\'') {
      IncrementCursor();
    } else {
      Error::Log::Log(Error::MsgId::RunawayCharLit, cursor.Location(), 0, 1);
    }

    RETURN_TERMINAL(Char, Char, Context::Value(result));
  } break;

  case '?':
    Error::Log::Log(Error::MsgId::InvalidCharQuestionMark, cursor.Location(), 0,
                    1);
    IncrementCursor();
    return Next();

  case '~':
    Error::Log::Log(Error::MsgId::InvalidCharTilde, cursor.Location(), 0, 1);
    IncrementCursor();
    return Next();

  case '_': UNREACHABLE;
  default: UNREACHABLE;
  }
}

#undef RETURN_NNT
#undef RETURN_TERMINAL
