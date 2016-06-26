#include "Lexer.h"

#ifndef ICARUS_UNITY
#include "Type/Type.h"
#endif

extern std::queue<std::string> file_queue;

#define RETURN_TERMINAL(term_type, ty, val)                                    \
  auto term_ptr           = new AST::Terminal;                                 \
  term_ptr->loc           = cursor.Location();                                              \
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

  std::string temp;
  std::getline(ifs, temp);

  pstr temp_blank;
  source_file_->lines.push_back(temp_blank); // Blank line since we 1-index.

  cursor.line_ = pstr(temp.c_str());
  source_file_->lines.push_back(cursor.line_);
  ++cursor.line_num_;
}

Lexer::~Lexer() { ifs.close(); }

void Lexer::IncrementCursor() {
  if (*cursor != '\0') {
    ++cursor.offset_;

  } else {
    assert(!ifs.eof());
    std::string temp;
    std::getline(ifs, temp);

    cursor.offset_            = 0;
    cursor.line_              = pstr(temp.c_str());
    ++cursor.line_num_;
    source_file_->lines.push_back(cursor.line_);
  }
}

void Lexer::SkipToEndOfLine() {
  while (*cursor != '\0') { ++cursor.offset_; }
}

static inline bool IsLower(char c) { return ('a' <= c && c <= 'z'); }
static inline bool IsUpper(char c) { return ('A' <= c && c <= 'Z'); }
static inline bool IsDigit(char c) { return ('0' <= c && c <= '9'); }
static inline bool IsAlpha(char c) { return IsLower(c) || IsUpper(c); }
static inline bool IsAlphaNumeric(char c) { return IsAlpha(c) || IsDigit(c); }
static inline bool IsAlphaOrUnderscore(char c) {
  return IsAlpha(c) || (c == '_');
}
static inline bool IsAlphaNumericOrUnderscore(char c) {
  return IsAlphaNumeric(c) || (c == '_');
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
    // TODO If you want to remove nullptr, and instead use
    // Context::Value(builtin::ord()), you can, but you need to also initialize
    // the global_module before you make a call to the lexer.
    RETURN_TERMINAL(Ord, Func(Char, Uint), nullptr);

  } else if (token == "ascii") {
    // TODO If you want to remove nullptr, and instead use
    // Context::Value(builtin::ascii()), you can, but you need to also
    // initialize the global_module before you make a call to the lexer.
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
    // TODO what about in a loop: "for i in 0..3
    do { IncrementCursor(); } while (IsDigit(*cursor));

    char old_char = *cursor;
    *cursor       = '\0';
    auto real_val = std::stod(cursor.line_.ptr + starting_offset);
    *cursor       = old_char;

    RETURN_TERMINAL(Real, Real, Context::Value(real_val));
  } break;

  default: {
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

  case '{': {
    IncrementCursor();
    if (*cursor == '{') {
      IncrementCursor();
      RETURN_NNT("{{", l_eval);

    } else {
      RETURN_NNT("{", l_brace);
    }
  } break;

  case '}': {
    IncrementCursor();
    if (*cursor == '}') {
      IncrementCursor();
      RETURN_NNT("}}", r_eval);

    } else {
      RETURN_NNT("}", r_brace);
    }
  } break;

  case '.': {
    IncrementCursor();
    if (*cursor == '.') {
      IncrementCursor();
      RETURN_NNT("..", dots);
    } else {
      RETURN_NNT(".", op_b);
    }
  } break;

  case '\\': {
    IncrementCursor();
    if (*cursor == '\\') {
      IncrementCursor();
      RETURN_NNT("", newline);
    } else {
      NOT_YET;
    }
  } break;

  case '#': {
    auto starting_offset = cursor.offset_ + 1; // Skip the '#' character.

    do { IncrementCursor(); } while (IsAlphaNumericOrUnderscore(*cursor));

    // TODO what if the hashtag is empty? what if it's not immediately followed
    // by
    // an identifier?

    char old_char   = *cursor;
    *cursor         = '\0';
    std::string tag = cursor.line_.ptr + starting_offset;
    *cursor         = old_char;

    RETURN_NNT(tag, hashtag);
  } break;

  case '+':
  case '*':
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
          Error::Log::Log(Error::Msg(Error::MsgId::RunawayMultilineComment,
                                     cursor.Location(), 0, 2));

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
    // TODO keep track of the first place it would make sense to end the string, just in case they forgot to end it.

    while (*cursor != '"' && *cursor != '\0') {
      if (*cursor == '\\') {
        IncrementCursor();
        switch (*cursor) {
        case '\'': {
          str_lit += '\'';
          TokenLocation loc = cursor.Location();
          --loc.offset;
          Error::Log::Log(Error::Msg(Error::MsgId::EscapedSingleQuoteInStringLit,
                                   loc, 0, 2));
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
          Error::Log::Log(
              Error::Msg(Error::MsgId::InvalidEscapeCharInCharLit, loc, 0, 2));

            str_lit += *cursor;
          } break;
        }
      } else {
        str_lit += *cursor;
      }

      IncrementCursor();
    }

    if (*cursor == '\0') {
      Error::Log::Log(
          Error::Msg(Error::MsgId::RunawayStringLit, cursor.Location(), 0, 1));
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
    case '\t': NOT_YET;
    case '\n':
    case '\r': {
                 // TODO how can you even get here?
      Error::Log::Log(
          Error::Msg(Error::MsgId::RunawayCharLit, cursor.Location(), 0, 1));

      RETURN_TERMINAL(Char, Char, Context::Value('\0'));
    }
    case '\\': {
      IncrementCursor();
      switch (*cursor) {
      case '\"': {
        result = '"';
        TokenLocation loc = cursor.Location();
        --loc.offset;
        Error::Log::Log(
            Error::Msg(Error::MsgId::EscapedDoubleQuoteInCharLit, loc, 0, 2));
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
        Error::Log::Log(
            Error::Msg(Error::MsgId::InvalidEscapeCharInCharLit, loc, 0, 2));
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
      Error::Log::Log(
          Error::Msg(Error::MsgId::RunawayCharLit, cursor.Location(), 0, 1));
    }

    RETURN_TERMINAL(Char, Char, Context::Value(result));
  } break;

  case '$': NOT_YET;
  case '?': NOT_YET;
  case '~': NOT_YET;

  case '_': UNREACHABLE;
  default: UNREACHABLE;
  }
}

#undef RETURN_NNT
#undef RETURN_TERMINAL
