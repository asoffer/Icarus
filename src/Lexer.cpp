#include "Lexer.h"
#include "Type.h"

extern std::queue<std::string> file_queue;

#define RETURN_TERMINAL(term_type, ty, tk)                                     \
  auto term_ptr           = new AST::Terminal;                                 \
  term_ptr->loc           = loc_;                                              \
  term_ptr->terminal_type = Language::Terminal::term_type;                     \
  term_ptr->type          = ty;                                                \
  term_ptr->token_        = tk;                                                \
                                                                               \
  term_ptr->set_node_type(Language::expression);                               \
  return term_ptr

namespace Language {
const std::map<std::string, NodeType> reserved_words = {
    {"if", reserved_if},         {"in", reserved_in},
    {"else", reserved_else},     {"case", reserved_case},
    {"for", reserved_for},       {"enum", reserved_enum},
    {"while", reserved_while},   {"free", reserved_free},
    {"import", reserved_import}, {"struct", reserved_struct},
    {"print", reserved_print}};
} // namespace Language

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
    loc_.offset = 0; // TODO not using this yet.
  }

// Lexer destructor:
//
// Closes the file opened by the constructor
Lexer::~Lexer() { file_.close(); }

// Get the next token
AST::Node *Lexer::Next() {
  int peek;

restart:
  // This label is used in the case that a space/tab character is encountered.
  // It allows us to repeat until we find a non-space/tab character.

  peek = file_.peek();

  // Delegate based on the next character in the file stream
  if (peek == EOF) {
    return AST::TokenNode::Eof(loc_);

  } else if (isnewline(peek)) {
    ++loc_.line_num;
    file_.get();
    return AST::TokenNode::Newline(loc_);

  } else if (std::isspace(peek)) {
    // Ignore space/tab characters by restarting
    file_.get();
    goto restart;

  } else if (std::isalpha(peek) || peek == '_') {
    return next_word();

  } else if (std::isdigit(peek)) {
    return next_number();

  } else if (peek == '"') {
    file_.get();
    file_queue.emplace("lib/string.ic");
    return next_string_literal();

  } else if (peek == '\'') {
    file_.get();
    return next_char_literal();

  } else if (std::ispunct(peek)) {
    return next_operator();

  } else {
    assert(false && "Lexer found a control character.");
  }
}

// The next token begins with an alpha character meaning that it is either a
// reserved word or an identifier.
AST::Node *Lexer::next_word() {
  assert((std::isalpha(file_.peek()) || file_.peek() == '_')
      && "Non-alpha character encountered as first character in next_word.");

  // Used to store the word
  std::string token;

  // Repeatedly add characters to the word so long as the word only uses
  // characters in the range a-z, A-Z, 0-9, and _ (and starts with an alpha
  // character).
  int peek;
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::isalnum(peek) || peek == '_');

  // Check if the word is a type primitive/literal and if so, build the
  // appropriate Node.
  for (const auto &type_lit : TypeSystem::Literals) {
    if (type_lit.first == token) {
      RETURN_TERMINAL(Type, Type_, token);
    }
  }

  if (token == "true") {
    RETURN_TERMINAL(True, Bool, "true");

  } else if (token == "false") {

    RETURN_TERMINAL(True, Bool, "false");

  } else if (token == "null") {
    RETURN_TERMINAL(Null, NullPtr, "null");

  } else if (token == "ord") {
    RETURN_TERMINAL(Ord, Func(Char, Uint), "ord");

  } else if (token == "ascii") {
    RETURN_TERMINAL(ASCII, Func(Uint, Char), "ascii");

  } else if (token == "alloc") {
    RETURN_TERMINAL(Alloc, DepType([](Type *t) { return Ptr(t); }), "alloc");

  } else if (token == "input") {
    RETURN_TERMINAL(Input, DepType([](Type *t) { return t; }), "input");

  } else if (token == "return") {
    auto term_ptr           = new AST::Terminal;
    term_ptr->loc           = loc_;
    term_ptr->terminal_type = Language::Terminal::Return;
    term_ptr->type          = Void;
    term_ptr->token_        = "return";

    term_ptr->set_node_type(Language::reserved_return);
    return term_ptr;

  } else if (token == "continue") {
    auto jmp = new AST::Jump(loc_, AST::Jump::JumpType::Continue);
    jmp->set_node_type(Language::reserved_continue);
    return jmp;

  } else if (token == "break") {
    auto jmp = new AST::Jump(loc_, AST::Jump::JumpType::Break);
    jmp->set_node_type(Language::reserved_break);
    return jmp;

  } else if (token == "repeat") {
    auto jmp = new AST::Jump(loc_, AST::Jump::JumpType::Repeat);
    jmp->set_node_type(Language::reserved_repeat);
    return jmp;

  } else if (token == "restart") {
    auto jmp = new AST::Jump(loc_, AST::Jump::JumpType::Restart);
    jmp->set_node_type(Language::reserved_restart);
    return jmp;
  }

  // Check if the word is reserved and if so, build the appropriate Node
  for (const auto &res : Language::reserved_words) {
    if (res.first == token) {
      auto nptr = new AST::TokenNode;
      *nptr     = AST::TokenNode(loc_, res.second, res.first);
      return nptr;
    }
  }

  if (token == "string") { file_queue.emplace("lib/string.ic"); }
  return new AST::Identifier(loc_, token);
}

AST::Node *Lexer::next_number() {
  assert(std::isdigit(file_.peek())
      && "Non-digit character encountered as first character in next_number.");

  // Used to store the number
  std::string token;

  // Add digits
  int peek;
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::isdigit(peek));

  if (peek == 'u' || peek == 'U') {
    // If the next character is a 'u' or a 'U', it's an integer literal. We can
    // ignore the character and return.
    file_.get();
    RETURN_TERMINAL(Uint, Uint, token);
  } else if (peek != '.') {
    // If the next character is not a period, we're looking at an integer and
    // can return.
    RETURN_TERMINAL(Int, Int, token);
  }

  // If the next character was a period, this is a non-integer. Add the period
  // and keep going with more digits.
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::isdigit(peek));

  RETURN_TERMINAL(Real, Real, token);
}

AST::Node *Lexer::next_operator() {
  // Sanity check:
  // We only call this function if the top character is punctuation
  int peek = file_.peek();
  assert(
      std::ispunct(peek) &&
      "Non-punct character encountered as first character in next_operator.");

  // In general, we're going to take all punctuation characters, lump them
  // together, and call that an operator. However, some operators are just one
  // character and should be taken by themselves.
  //
  // For example, the characters '(', ')', '[', ']', '{', '}', '"', '\'', if
  // encountered should be considered on their own.
  switch (peek) {
#define CASE(character, str, name)                                             \
  case character:                                                              \
    file_.get();                                                               \
    return new AST::TokenNode(loc_, Language::name, str)
    CASE('`', "`", tick);
    CASE('@', "@", dereference);
    CASE(',', ",", chainop);
    CASE(';', ";", semicolon);
    CASE('(', "(", left_paren);
    CASE(')', ")", right_paren);
    CASE('[', "[", left_bracket);
    CASE(']', "]", right_bracket);
    CASE('{', "{", left_brace);
    CASE('}', "}", right_brace);
#undef CASE

  case '.': {
    file_.get();
    if (file_.peek() == '.') {
      file_.get();
      return new AST::TokenNode(loc_, Language::dots, "..");
    } else {
      return new AST::TokenNode(loc_, Language::dot, ".");
    }
  }
  case '#': return next_hashtag();
  case '"':
  case '\'': {
    assert(false);
  }
  }

  if (peek == '/') { return next_given_slash(); }

  // Cannot have '-' in this list because of '->'
  // The case '/' is missing because it has special the cases // and /* to deal
  // with
  char lead_char = 0;
  Language::NodeType node_type;
  switch (peek) {
  case '+':
  case '*':
  case '%':
    lead_char = static_cast<char>(peek);
    node_type = Language::binop;
    break;
  case '<':
  case '>':
    lead_char = static_cast<char>(peek);
    node_type = Language::chainop;
    break;
  default: lead_char = 0;
  }

  if (lead_char != 0) {
    file_.get();
    peek = file_.peek();

    std::string tok(1, lead_char);
    if (peek == '=') {
      tok += "=";
      file_.get();
    }

    if (node_type == Language::binop && peek == '=') {
      node_type = Language::assign_operator;
    }

   return new AST::TokenNode(loc_, node_type, tok);
  }

  if (peek == ':') {
    file_.get();
    peek = file_.peek();

    if (peek == '=') {
      file_.get();
      return new AST::TokenNode(loc_, Language::declop, ":=");

    } else if (peek == '>') {
      file_.get();
      return new AST::TokenNode(loc_, Language::binop, ":>");
 
    } else {
      return new AST::TokenNode(loc_, Language::declop, ":");
    }
  }

  if (peek == '|' || peek == '^' || peek == '&') {
    char past_peek = static_cast<char>(peek);

    file_.get();
    peek = file_.peek();

    if (peek == '=') {
      std::string tok = "_=";
      tok[0] = past_peek;
      file_.get();
      return new AST::TokenNode(loc_, Language::assign_operator, tok);

    } else {
      return new AST::TokenNode(
          loc_, (past_peek == '&' ? Language::indirection : Language::chainop),
          std::string(1, past_peek));
    }
  }

  if (peek == '!') {
    file_.get();
    peek = file_.peek();

    if (peek == '=') {
      file_.get();
      return new AST::TokenNode(loc_, Language::chainop, "!=");
    } else {
      return new AST::TokenNode(loc_, Language::not_operator, "!");
    }
  }

  if (peek == '-' || peek == '=') {
    char lead_char = static_cast<char>(peek);
    file_.get();
    peek = file_.peek();

    if (peek == '=') {
      file_.get();

      Language::NodeType node_type =
          (lead_char == '-' ? Language::assign_operator : Language::chainop);
      return new AST::TokenNode(loc_, node_type, std::string(1, lead_char) + "=");
      
    } else if (peek == '>') {
      file_.get();
      if (lead_char == '-') {
        return new AST::TokenNode(loc_, Language::fn_arrow, "->");
      } else {
        return new AST::TokenNode(loc_, Language::rocket_operator, "=>");
      }
    } else {
      if (lead_char == '-') {
        if (peek == '-') {
          file_.get();
          RETURN_TERMINAL(Hole, Unknown, "--");
        }
        return new AST::TokenNode(loc_, Language::negation, "-");
      } else {
        return new AST::TokenNode(loc_, Language::assign_operator, "=");
      }
    }
  }

  // If the first character isn't one of the specific ones mentioned above, read
  // in as many characters as possible.
  std::string token;
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::ispunct(peek));

  return new AST::TokenNode(loc_, Language::binop, token);
}

AST::Node *Lexer::next_string_literal() {
  int peek = file_.peek();
  std::string str_lit = "";

  // Repeat until you see a double-quote to end the string, or a newline
  // (which designates an error where they forgot to end the string)
  while (!(peek == '"' || isnewline(peek))) {
    // If you see a backslash, the next character is escaped
    if (peek == '\\') {
      file_.get();
      peek = file_.peek();
      switch (peek) {
      case '\\': str_lit += '\\'; break;
      case '"': str_lit += '"'; break;
      case 'n': str_lit += '\n'; break;
      case 'r': str_lit += '\r'; break;
      case 't': str_lit += '\t'; break;
      default: {
        error_log.log(loc_, "The sequence `\\" +
                                     std::to_string(static_cast<char>(peek)) +
                                     "` is not an escape character.");

        str_lit += static_cast<char>(peek);
        break;
      }
      }
      file_.get();
    } else {
      str_lit += static_cast<char>(file_.get());
    }

    peek = file_.peek();
  }

  if (peek == '"') {
    // Ignore the last quotation mark if it exists
    file_.get();
  }
  else {
    error_log.log(loc_,
        "String literal is not closed before the end of the line.");
  }

  // TODO Why not String instead of Unknown for the type?
  RETURN_TERMINAL(StringLiteral, Unknown, str_lit);
}

AST::Node *Lexer::next_char_literal() {
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
        return new AST::TokenNode(loc_, Language::newline);
      }
    case '\\':
      {
        file_.get();
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
                     error_log.log(loc_,
                         "The specified character is not an escape character.");
          output_char = static_cast<char>(peek);
        }

        break;
      }
    default:
      {
        output_char = static_cast<char>(peek);
      }
  }

  file_.get();
  peek = file_.peek();

  if (peek == '\'') {
    file_.get();
  } else {
    error_log.log(loc_, "Character literal must be followed by a single-quote.");
  }

  RETURN_TERMINAL(Char, Char, std::string(1, output_char));
}

AST::Node *Lexer::next_given_slash() {
  int peek = file_.peek();
  assert(peek == '/' && "Non-slash character encountered as first character in next_given_slash.");

  file_.get();
  peek = file_.peek();

  // If the first two characters are '//', then it's a single-line comment
  if (peek == '/') {
    std::string token = "";

    // Add characters while we're not looking at a newline
    do {
      token += static_cast<char>(file_.get());
      peek = file_.peek();
    } while (!isnewline(peek));

    return new AST::TokenNode(loc_, Language::comment, token);
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
      peek = file_.get();

      if (!*this) {  // If we're at the end of the stream
        error_log.log(loc_, "File ended during multi-line comment.");
        return new AST::TokenNode(loc_, Language::comment, "");
      }

      if (prepeek == '/' && peek == '*') {
        ++comment_layer;

      } else if (prepeek == '*' && peek == '/') {
        --comment_layer;
      }
    }

    return new AST::TokenNode(loc_, Language::comment, "MULTILINE COMMENT");
  }

  if (peek == '=') {
    file_.get();
    return new AST::TokenNode(loc_, Language::assign_operator, "/=");
  }

  return new AST::TokenNode(loc_, Language::binop, "/");
}

AST::Node *Lexer::next_hashtag() {
  int peek = file_.peek();
  assert(peek == '#' && "Non-hash character encountered as first character in next_hashtag.");
  file_.get();

  auto nptr = next_word();
  std::string tag = nptr->token();
  delete nptr;

  return new AST::TokenNode(loc_, Language::hashtag, tag);
}

#undef RETURN_TERMINAL
