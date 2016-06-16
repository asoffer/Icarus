#include "Lexer.h"

#ifndef ICARUS_UNITY
#include "Type/Type.h"
#endif

extern std::queue<std::string> file_queue;
namespace builtin {
llvm::Function *ord();
llvm::Function *ascii();
} // namespace builtin

#define RETURN_TERMINAL(term_type, ty, val)                                \
  auto term_ptr           = new AST::Terminal;                                 \
  term_ptr->loc           = loc_;                                              \
  term_ptr->terminal_type = Language::Terminal::term_type;                     \
  term_ptr->type          = ty;                                                \
  term_ptr->value         = val;                                               \
  return NNT(term_ptr, Language::expr);

#define RETURN_NNT(tk, nt)                                                     \
  return NNT(new AST::TokenNode(loc_, tk), Language::nt);
namespace TypeSystem {
extern std::map<std::string, Type *> Literals;
} // namespace TypeSystem

// Local function for recognizing newlines a la std::isalpha, etc.
bool isnewline(int n) { return n == '\n' || n == '\r'; }

// Take a filename as a string or a C-string and opens the named file
Lexer::Lexer(const std::string &file_name)
    : file_(file_name, std::ifstream::in) {
    loc_.file = file_name.c_str();
    loc_.line_num = 1;
    loc_.offset = 0;
  }

// Lexer destructor:
//
// Closes the file opened by the constructor
Lexer::~Lexer() { file_.close(); }

int Lexer::GetChar() {
  ++loc_.offset;
  int output = file_.get();
  return output;
}

// Get the next token
NNT Lexer::Next() {
  int peek;

restart:
  // This label is used in the case that a space/tab character is encountered.
  // It allows us to repeat until we find a non-space/tab character.

  peek = file_.peek();

  // Delegate based on the next character in the file stream
  if (peek == EOF) {
    RETURN_NNT("", eof);

  } else if (isnewline(peek)) {
    ++loc_.line_num;
    GetChar();
    loc_.offset = 0;
    RETURN_NNT("", newline);

  } else if (std::isspace(peek)) {
    // Ignore space/tab characters by restarting
    GetChar();
    goto restart;

  } else if (std::isalpha(peek) || peek == '_') {
    return next_word();

  } else if (std::isdigit(peek)) {
    return next_number();

  } else if (peek == '"') {
    GetChar();
    file_queue.emplace("lib/string.ic");
    return next_string_literal();

  } else if (peek == '\'') {
    GetChar();
    return next_char_literal();

  } else if (std::ispunct(peek)) {
    return next_operator();

  } else {
    assert(false && "Lexer found a control character.");
  }
}

// The next token begins with an alpha character meaning that it is either a
// reserved word or an identifier.
NNT Lexer::next_word() {
  assert((std::isalpha(file_.peek()) || file_.peek() == '_')
      && "Non-alpha character encountered as first character in next_word.");

  // Used to store the word
  std::string token;

  // Repeatedly add characters to the word so long as the word only uses
  // characters in the range a-z, A-Z, 0-9, and _ (and starts with an alpha
  // character).
  int peek;
  do {
    token += (char)GetChar();
    peek = file_.peek();
  } while (std::isalnum(peek) || peek == '_');

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

  } else if (token == "print" || token == "import" || token == "free" || token == "eval") {
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

NNT Lexer::next_number() {
  assert(std::isdigit(file_.peek())
      && "Non-digit character encountered as first character in next_number.");

  // Used to store the number
  std::string token;

  // Add digits
  int peek;
  do {
    token += (char)GetChar();
    peek = file_.peek();
  } while (std::isdigit(peek));

  if (peek == 'u' || peek == 'U') {
    // If the next character is a 'u' or a 'U', it's an integer literal. We can
    // ignore the character and return.
    GetChar();
    RETURN_TERMINAL(Uint, Uint, Context::Value(std::stoul(token))); // TODO nullptr
  } else if (peek != '.') {
    // If the next character is not a period, we're looking at an integer and
    // can return.
    RETURN_TERMINAL(Int, Int, Context::Value(std::stol(token))); // TODO nullptr
  }

  // If the next character was a period, this is a non-integer. Add the period
  // and keep going with more digits.
  do {
    token += (char)GetChar();
    peek = file_.peek();
  } while (std::isdigit(peek));

  RETURN_TERMINAL(Real, Real, Context::Value(std::stod(token))); // TODO nullptr
}

NNT Lexer::next_operator() {
  // Sanity check:
  // We only call this function if the top character is punctuation
  int peek = file_.peek();
  assert(
      std::ispunct(peek) &&
      "Non-punct character encountered as first character in next_operator.");

#define CASE(character, str, name)                                             \
  case character: GetChar(); RETURN_NNT(str, name)

  switch (peek) {
    CASE('`', "`", op_bl);
    CASE('@', "@", op_l);
    CASE(',', ",", comma);
    CASE(';', ";", semicolon);
    CASE('(', "(", l_paren);
    CASE(')', ")", r_paren);
    CASE('[', "[", l_bracket);
    CASE(']', "]", r_bracket);
    CASE('{', "{", l_brace);
    CASE('}', "}", r_brace);

  case '.': {
    GetChar();
    if (file_.peek() == '.') {
      GetChar();
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
    GetChar();
    peek = file_.peek();
    if (peek == '\\') {
      GetChar();
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
    GetChar();
    peek = file_.peek();

    std::string tok(1, lead_char);
    if (peek == '=') {
      tok += "=";
      GetChar();
    }

    RETURN_NNT(tok, op_b);
  }
  default: lead_char = 0;
  }

  if (peek == ':') {
    GetChar();
    peek = file_.peek();

    if (peek == '=') {
      GetChar();
      RETURN_NNT(":=", op_b);

    } else if (peek == '>') {
      GetChar();
      RETURN_NNT(":>", op_b);

    } else {
      RETURN_NNT(":", colon);
    }
  }

  if (peek == '|' || peek == '^' || peek == '&') {
    char past_peek = (char)peek;

    GetChar();
    peek = file_.peek();

    if (peek == '=') {
      std::string tok = "_=";
      tok[0] = past_peek;
      GetChar();
      RETURN_NNT(tok, op_b);

    } else if (past_peek == '&') {
        RETURN_NNT("&", op_bl);

    } else {
      RETURN_NNT(std::string(1, past_peek), op_b);
    }
  }

  if (peek == '!') {
    GetChar();
    peek = file_.peek();

    if (peek == '=') {
      GetChar();
      RETURN_NNT("!=", op_b);
    } else {
      RETURN_NNT("!", op_l);
    }
  }

  if (peek == '-' || peek == '=') {
    char lead_char = (char)peek;
    GetChar();
    peek = file_.peek();

    if (peek == '=') {
      GetChar();

      RETURN_NNT((std::string(1, lead_char) + "="), op_b);

    } else if (peek == '>') {
      GetChar();
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
          GetChar();
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
    token += (char)GetChar();
    peek = file_.peek();
  } while (std::ispunct(peek));

  RETURN_NNT(token, op_b);
}

NNT Lexer::next_string_literal() {
  int peek = file_.peek();
  std::string str_lit = "";

  // Repeat until you see a double-quote to end the string, or a newline
  // (which designates an error where they forgot to end the string)
  while (!(peek == '"' || isnewline(peek))) {
    // If you see a backslash, the next character is escaped
    if (peek == '\\') {
      GetChar();
      peek = file_.peek();
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
      GetChar();
    } else {
      str_lit += (char)GetChar();
    }

    peek = file_.peek();
  }

  if (peek == '"') {
    // Ignore the last quotation mark if it exists
    GetChar();
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
  int peek = file_.peek();

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
        GetChar();
        peek = file_.peek();
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

  GetChar();
  peek = file_.peek();

  if (peek == '\'') {
    GetChar();
  } else {
    error_log.log(loc_, "Character literal must be followed by a single-quote.");
  }

  RETURN_TERMINAL(Char, Char, Context::Value(output_char));
}

NNT Lexer::next_given_slash() {
  int peek = file_.peek();
  assert(peek == '/' && "Non-slash character encountered as first character in next_given_slash.");

  GetChar();
  peek = file_.peek();

  // If the first two characters are '//', then it's a single-line comment
  if (peek == '/') {
    std::string token = "";

    // Add characters while we're not looking at a newline
    do {
      token += (char)GetChar();
      peek = file_.peek();
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
      peek = GetChar();

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
    GetChar();
    RETURN_NNT("/=", op_b);
  }

  RETURN_NNT("/", op_b);
}

NNT Lexer::next_hashtag() {
  int peek = file_.peek();
  assert(peek == '#' &&
         "Non-hash character encountered as first character in next_hashtag.");
  GetChar();

  std::string tag;

  auto nptr = next_word();
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
