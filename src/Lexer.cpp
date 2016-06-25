#include "Lexer.h"

#ifndef ICARUS_UNITY
#include "Type/Type.h"
#endif

extern std::queue<std::string> file_queue;

#define RETURN_TERMINAL(term_type, ty, val)                                    \
  auto term_ptr           = new AST::Terminal;                                 \
  term_ptr->loc           = loc_;                                              \
  term_ptr->terminal_type = Language::Terminal::term_type;                     \
  term_ptr->type          = ty;                                                \
  term_ptr->value = val;                                                       \
  return NNT(term_ptr, Language::expr);

#define RETURN_NNT(tk, nt)                                                     \
  return NNT(new AST::TokenNode(loc_, tk), Language::nt);
namespace TypeSystem {
extern std::map<std::string, Type *> Literals;
} // namespace TypeSystem

// Local function for recognizing newlines a la std::isalpha, etc.
bool isnewline(int n) { return n == '\n' || n == '\r'; }

// Take a filename as a string or a C-string and opens the named file
Lexer::Lexer(const std::string &file_name) : ifs(file_name, std::ifstream::in) {
  loc_.file     = file_name.c_str();
  loc_.line_num = 1;
  loc_.offset   = 0;

  std::string temp;
  std::getline(ifs, temp);

  cursor.line_ = pstr(temp.c_str());
  lines.push_back(cursor.line_);
  ++cursor.line_num_;
}

Lexer::~Lexer() { ifs.close(); }

void Lexer::IncrementCursor() {
  if (*cursor != '\0') {
    ++cursor.offset_;
    ++loc_.offset;

  } else {
    assert(!ifs.eof());
    std::string temp;
    std::getline(ifs, temp);

    cursor.offset_            = 0;
    cursor.line_              = pstr(temp.c_str());
    ++cursor.line_num_;
    lines.push_back(cursor.line_);
  }
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
  case '"':
    IncrementCursor();
    file_queue.emplace("lib/string.ic");
    return next_string_literal();

  case '\'': IncrementCursor(); return next_char_literal();
  case '\0': IncrementCursor(); RETURN_NNT("", newline);
  default: return next_operator();
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
    term_ptr->loc           = loc_;
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
  return NNT(new AST::Identifier(loc_, token), Language::expr);
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

NNT Lexer::next_operator() {
  // Sanity check:
  // We only call this function if the top character is punctuation
  int peek = *cursor;
  assert(
      std::ispunct(peek) &&
      "Non-punct character encountered as first character in next_operator.");

#define CASE(character, str, name)                                             \
  case character: IncrementCursor(); RETURN_NNT(str, name)

  switch (peek) {
    CASE('`', "`", op_bl);
    CASE('@', "@", op_l);
    CASE(',', ",", comma);
    CASE(';', ";", semicolon);
    CASE('(', "(", l_paren);
    CASE(')', ")", r_paren);
    CASE('[', "[", l_bracket);
    CASE(']', "]", r_bracket);
  case '{': {
    IncrementCursor();
    if (*cursor == '{') {
      IncrementCursor();
      RETURN_NNT("{{", l_eval);

    } else {
      RETURN_NNT("{", l_brace);
    }
  }
  case '}': {
    IncrementCursor();
    if (*cursor == '}') {
      IncrementCursor();
      RETURN_NNT("}}", r_eval);

    } else {
      RETURN_NNT("}", r_brace);
    }
  }
  case '.': {
    IncrementCursor();
    if (*cursor == '.') {
      IncrementCursor();
      RETURN_NNT("..", dots);
    } else {
      RETURN_NNT(".", op_b);
    }
  }
  case '#': return next_hashtag();
  case '"':
  case '\'': {
    assert(false);
  }
  }

#undef CASE
  if (peek == '\\') {
    IncrementCursor();
    peek = *cursor;
    if (peek == '\\') {
      IncrementCursor();
      RETURN_NNT("", newline);
    } else {
      NOT_YET;
    }
  }

  if (peek == '/') { return next_given_slash(); }

  // Cannot have '-' in this list because of '->'
  // The case '/' is missing because it has special the cases // and /* to deal
  // with
  char lead_char = 0;
  switch (peek) {
  case '+':
  case '*':
  case '%':
  case '<':
  case '>': {
    lead_char = (char)peek;
    IncrementCursor();
    peek = *cursor;

    std::string tok(1, lead_char);
    if (peek == '=') {
      tok += "=";
      IncrementCursor();
    }

    RETURN_NNT(tok, op_b);
  }
  default: lead_char = 0;
  }

  if (peek == ':') {
    IncrementCursor();
    peek = *cursor;

    if (peek == '=') {
      IncrementCursor();
      RETURN_NNT(":=", op_b);

    } else if (peek == '>') {
      IncrementCursor();
      RETURN_NNT(":>", op_b);

    } else {
      RETURN_NNT(":", colon);
    }
  }

  if (peek == '|' || peek == '^' || peek == '&') {
    char past_peek = (char)peek;

    IncrementCursor();
    peek = *cursor;

    if (peek == '=') {
      std::string tok = "_=";
      tok[0] = past_peek;
      IncrementCursor();
      RETURN_NNT(tok, op_b);

    } else if (past_peek == '&') {
        RETURN_NNT("&", op_bl);

    } else {
      RETURN_NNT(std::string(1, past_peek), op_b);
    }
  }

  if (peek == '!') {
    IncrementCursor();
    peek = *cursor;

    if (peek == '=') {
      IncrementCursor();
      RETURN_NNT("!=", op_b);
    } else {
      RETURN_NNT("!", op_l);
    }
  }

  if (peek == '-' || peek == '=') {
    char lead_char = (char)peek;
    IncrementCursor();
    peek = *cursor;

    if (peek == '=') {
      IncrementCursor();

      RETURN_NNT((std::string(1, lead_char) + "="), op_b);

    } else if (peek == '>') {
      IncrementCursor();
      if (lead_char == '-') {
        auto nptr = new AST::TokenNode(loc_, "->");
        nptr->op  = Language::Operator::Arrow;
        return NNT(nptr, Language::fn_arrow);

      } else {
        RETURN_NNT("=>", op_b);
      }
    } else {
      if (lead_char == '-') {
        if (peek == '-') {
          IncrementCursor();
          RETURN_TERMINAL(Hole, Unknown, nullptr); // TODO nullptr
        }
        RETURN_NNT("-", op_bl);
      } else {

        RETURN_NNT("=", eq);
      }
    }
  }

  // If the first character isn't one of the specific ones mentioned above, read
  // in as many characters as possible.
  std::string token;
  do {
    token += *cursor;
    IncrementCursor();
    peek = *cursor;
  } while (std::ispunct(peek));

  RETURN_NNT(token, op_b);
}

NNT Lexer::next_string_literal() {
  int peek = *cursor;
  std::string str_lit = "";

  // Repeat until you see a double-quote to end the string, or a newline
  // (which designates an error where they forgot to end the string)
  while (!(peek == '"' || isnewline(peek))) {
    // If you see a backslash, the next character is escaped
    if (peek == '\\') {
      IncrementCursor();
      peek = *cursor;
      switch (peek) {
      case '\\': str_lit += '\\'; break;
      case '"': str_lit += '"'; break;
      case 'n': str_lit += '\n'; break;
      case 'r': str_lit += '\r'; break;
      case 't': str_lit += '\t'; break;
      default: {
        error_log.log(loc_, "The sequence `\\" + std::to_string((char)peek) +
                                "` is not an escape character.");

        str_lit += (char)peek;
        break;
      }
      }
      IncrementCursor();
    } else {
      str_lit += *cursor;
      IncrementCursor();
    }

    peek = *cursor;
  }

  if (peek == '"') {
    // Ignore the last quotation mark if it exists
    IncrementCursor();
  }
  else {
    error_log.log(loc_,
        "String literal is not closed before the end of the line.");
  }

  // TODO Why not String instead of Unknown for the type?

  char *cstr = new char[str_lit.size() + 1];
  std::strcpy(cstr, str_lit.c_str());
  RETURN_TERMINAL(StringLiteral, Unknown, Context::Value(cstr)); // TODO nullptr
}

NNT Lexer::next_char_literal() {
  int peek = *cursor;

  char output_char;

  // TODO 
  // 1. deal with the case of a tab character literally between single quotes.
  // 2. robust error handling
  switch(peek) {
    case '\n':
    case '\r':
      {
        error_log.log(loc_, "Cannot use newline inside a character-literal.");
        RETURN_NNT("", newline);
      }
    case '\\':
      {
        IncrementCursor();
        peek = *cursor;
        switch (peek) {
          case '\'': output_char = '\''; break;
          case '\"': output_char = '"';
                     error_log.log(loc_,
                         "Warning: the character '\"' does not need to be escaped.");
                     break;
          case '\\': output_char = '\\'; break;
          case 't':  output_char = '\t'; break;
          case 'n':  output_char = '\n'; break;
          case 'r':  output_char = '\r'; break;
          default:
            error_log.log(
                loc_, "The specified character is not an escape character.");
            output_char = (char)peek;
        }

        break;
      }
      default: { output_char = (char)peek; }
  }

  IncrementCursor();
  peek = *cursor;

  if (peek == '\'') {
    IncrementCursor();
  } else {
    error_log.log(loc_, "Character literal must be followed by a single-quote.");
  }

  RETURN_TERMINAL(Char, Char, Context::Value(output_char));
}

NNT Lexer::next_given_slash() {
  int peek = *cursor;
  assert(peek == '/' && "Non-slash character encountered as first character in next_given_slash.");

  IncrementCursor();
  peek = *cursor;

  // If the first two characters are '//', then it's a single-line comment
  if (peek == '/') {
    std::string token = "";

    // Add characters while we're not looking at a newline
    do {
      token += *cursor;
      IncrementCursor();
      peek = *cursor;
    } while (!isnewline(peek));

    RETURN_NNT(token, comment);
  }

  // If the first two characters are '/*' then start a multi-line comment.
  // Multi-line comments should nest
  if (peek == '*') {
    size_t comment_layer = 1;
    
    // This is intentionally not set to '/' (which must be the character
    // preceeding peek at this point because we don't want to count it in the
    // loop more than once). 
    int prepeek = 0;

    while (comment_layer != 0) {
      prepeek = peek;
      peek = *cursor;
      IncrementCursor();

      if (isnewline(peek)) { ++loc_.line_num; loc_.offset = 0; }

      if (!*this) {  // If we're at the end of the stream
        error_log.log(loc_, "File ended during multi-line comment.");
        RETURN_NNT("", comment);
      }

      if (prepeek == '/' && peek == '*') {
        ++comment_layer;

      } else if (prepeek == '*' && peek == '/') {
        --comment_layer;
      }
    }

    RETURN_NNT("", comment);
  }

  if (peek == '=') {
    IncrementCursor();
    RETURN_NNT("/=", op_b);
  }

  RETURN_NNT("/", op_b);
}

NNT Lexer::next_hashtag() {
  int peek = *cursor;
  assert(peek == '#' &&
         "Non-hash character encountered as first character in next_hashtag.");
  IncrementCursor();

  std::string tag;

  auto nptr = NextWord();
  if (nptr.node->is_identifier()) {
    tag = ((AST::Identifier *)nptr.node)->token;

  } else if (nptr.node->is_token_node()) {
    tag = ((AST::TokenNode *)nptr.node)->token;

  } else {
    assert(false);
  }
  delete nptr.node;

  RETURN_NNT(tag, hashtag);
}

#undef RETURN_NNT
#undef RETURN_TERMINAL
