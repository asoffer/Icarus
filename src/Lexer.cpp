#include "Lexer.h"
#include "Language.h"
#include <map>

// Local function for recognizing newlines a la std::isalpha, etc.
bool isnewline(int n) {
  return n == static_cast<int>('\n') || n == static_cast<int>('\r');
}

// Lexer constructors:
//
// Take a filename as a string or a C-string and opens the named file
Lexer::Lexer(const char* file_name) :
  file_name_(file_name),
  file_(file_name, std::ifstream::in) {
  }

Lexer::Lexer(const std::string& file_name) : Lexer(file_name.c_str()) {}

// Lexer destructor:
//
// Closes the file opened by the constructor
Lexer::~Lexer() { file_.close(); }

// Get the next token
Lexer& operator>>(Lexer& lexer, AST::Node& node) {
  int peek;

restart:
  // This label is used in the case that a space/tab character is encountered.
  // It allows us to repeat until we find a non-space/tab character.

  peek = lexer.file_.peek();

  // Delegate based on the next character in the file stream
  if (peek == EOF) {
    node = AST::Node::eof_node();
  }
  else if (isnewline(peek)) {
    node = AST::Node::newline_node();
    lexer.file_.get();
  }
  else if (std::isspace(peek)) {
    // Ignore space/tab characters by restarting
    lexer.file_.get();
    goto restart;
  }
  else if (std::isalpha(peek)) {
    node = lexer.next_word();
  }
  else if (std::isdigit(peek)) {
    node = lexer.next_number();
  }
  else if (std::ispunct(peek)) {
    node = lexer.next_operator();
  }
#ifdef DEBUG
  else {
    throw "Lexer found a control character.";
  }
#endif

  return lexer;
}

// The next token begins with an alpha character meaning that it is either a
// reserved word or an identifier.
AST::Node Lexer::next_word() {
#ifdef DEBUG
  // Sanity check:
  // We only call this function if the top character is an alpha character
  if (!std::isalpha(file_.peek()))
    throw "Non-alpha character encountered as first character in next_word.";
#endif

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

  // Check if the word is reserved and if so, build the appropriate Node
  for (const auto& res : Language::reserved_words) {
    if (res.first == token) {
      return AST::Node(res.second);
    }
  }

  for (const auto& type_lit : Type::Literals) {
    if (type_lit.first == token) {
      return AST::Node(Language::type_literal, token);
    }
  }

  // It's an identifier
  return AST::Node(Language::identifier, token);
}

AST::Node Lexer::next_number() {
#ifdef DEBUG
  // Sanity check:
  // We only call this function if the top character is a number character
  if (!std::isdigit(file_.peek()))
    throw "Non-digit character encountered as first character in next_number.";
#endif

  // Used to store the number
  std::string token;

  // Add digits
  int peek;
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::isdigit(peek));

  // If the next character is not a period, we're looking at an integer and can
  // return
  if (peek != static_cast<int>('.')) {
    return AST::Node(Language::integer, token);
  }

  // If the next character was a period, this is a non-integer. Add the period
  // and keep going with more digits.
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::isdigit(peek));

  return AST::Node(Language::real, token);
}

AST::Node Lexer::next_operator() {
  // Sanity check:
  // We only call this function if the top character is punctuation
  int peek = file_.peek();
#ifdef DEBUG
  if (!std::ispunct(peek))
    throw "Non-punct character encountered as first character in next_operator.";
#endif

  // In general, we're going to take all punctuation characters, lump them
  // together, and call that an operator. However, some operators are just one
  // character and should be taken by themselves.
  //
  // For example, the characters '(', ')', '[', ']', '{', '}', '"', '\'', if
  // encountered should be considered on their own.
  switch (peek) {
    case static_cast<int>('('):
      {
        file_.get();
        return AST::Node(Language::left_paren);
      }
    case static_cast<int>(')'):
      {
        file_.get();
        return AST::Node(Language::right_paren);
      }
    case static_cast<int>('{'):
      {
        file_.get();
        return AST::Node(Language::left_brace);
      }
    case static_cast<int>('}'):
      {
        file_.get();
        return AST::Node(Language::right_brace);
      }
    case static_cast<int>('['):
      {
        file_.get();
        return AST::Node(Language::left_bracket);
      }
    case static_cast<int>(']'):
      {
        file_.get();
        return AST::Node(Language::right_bracket);
      }
    case static_cast<int>('"'):
      {
        file_.get();
        return next_string_literal();
      }
      // TODO(andy) single-quote character
  }

  // If the first character isn't one of the specific ones mentioned above, read
  // in as many characters as possible.
  //
  // TODO it's possible to write 'a+=-b' and this will be lexed as a +=- b
  // rather than a += - b. This needs to be fixed.
  std::string token;
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::ispunct(peek));

  // If it's ':' or '=' treat them specially
  if (token == ":") {
    return AST::Node(Language::decl_operator, ":");
  } else if (token == "=") {
    return AST::Node(Language::assign_operator, "=");
  }

  // If it's exactly one character in length, there's nothing more to do
  if (token.size() == 1) {
    return AST::Node(Language::generic_operator, token);
  }

  // If the first two characters are '//', then it's a single-line comment
  if (token[0] == '/' && token[1] == '/') {
    // From here on, the token represents not the // operator, but the text of
    // the comment. Thus, we drop the first two characters
    token = token.substr(2);

    // Add characters while we're not looking at a newline
    do {
      token += static_cast<char>(file_.get());
      peek = file_.peek();
    } while (!isnewline(peek));

    return AST::Node(Language::comment, token);
  }

  // If the first two characters are '=>' use the fat-arrow
  // FIXME , this simply ignores punctuation following '=>'
  if (token[0] == '=' && token[1] == '>') {
    return AST::Node(Language::fat_arrow, "=>");
  }

  return AST::Node(Language::generic_operator, token);
}

AST::Node Lexer::next_string_literal() {
  int peek = file_.peek();
  std::string str_lit = "";

  // Repeat until you see a double-quote to end the string, or a newline
  // (which designates an error where they forgot to end the string)
  while (!(peek == static_cast<int>('"') || isnewline(peek))) {
    // If you see a backslash, the next character is escaped
    if (peek == static_cast<int>('\\')) {
      file_.get();
      peek = file_.peek();
      switch (peek) {
        case '\\':
          str_lit += '\\'; break;
        case '"':
          str_lit += '"'; break;
        case 'n':
          str_lit += '\n'; break;
        case 'r':
          str_lit += '\r'; break;
        case 't':
          str_lit += '\t'; break;
        default:
          break; // TODO Error invalid escaped character
      }
      file_.get();
    } else {
      str_lit += static_cast<char>(file_.get());
    }

    peek = file_.peek();
  }

  if (peek == static_cast<int>('"')) {
    // Ignore the last quotation mark if it exists
    file_.get();
  }
  else {
    // TODO error: you forgot to end the string
  }

  return AST::Node::string_literal_node(str_lit);
}
