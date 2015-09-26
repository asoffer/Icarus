#include "Lexer.h"
#include <map>

bool isnewline(int n) {
  return n == '\n' || n == '\r';
}

Lexer::Lexer(const char* file_name) :
  file_name_(file_name),
  file_(file_name, std::ifstream::in) {
  }

Lexer::~Lexer() {
  file_.close();
}

Lexer& operator>>(Lexer& lexer, AST::Node& node) {
  int peek;

restart:
  peek = lexer.file_.peek();

  if (peek == EOF) {
    node = AST::Node::eof_node();
  }
  else if (isnewline(peek)) {
    node = AST::Node::newline_node();
    lexer.file_.get();
  }
  else if (std::isspace(peek)) {
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


AST::Node Lexer::next_word() {
#ifdef DEBUG
  if (!std::isalpha(file_.peek()))
    throw "Non-alpha character encountered as first character in next_word.";
#endif

  std::string token;

  int peek;
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::isalnum(peek) || peek == '_');

  // TODO(andy) This should definitely be moved somewhere more central
  // eventually
  static std::map<std::string, AST::Node::Type> reserved_words = {
    { "if",       AST::Node::reserved_if },
    { "else",     AST::Node::reserved_else },
    { "case",     AST::Node::reserved_case },
    { "loop",     AST::Node::reserved_loop },
    { "while",    AST::Node::reserved_while },
    { "break",    AST::Node::reserved_break },
    { "continue", AST::Node::reserved_continue },
    { "return",   AST::Node::reserved_return }
  };

  for (const auto& res : reserved_words) {
    if (res.first == token) {
      return AST::Node(res.second);
    }
  }

  return AST::Node(AST::Node::identifier, token);
}

AST::Node Lexer::next_number() {
#ifdef DEBUG
  if (!std::isdigit(file_.peek()))
    throw "Non-digit character encountered as first character in next_number.";
#endif

  std::string token;

  int peek;
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::isdigit(peek));

  if (peek != static_cast<int>('.')) {
    return AST::Node(AST::Node::integer, token);
  }

  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::isdigit(peek));

  return AST::Node(AST::Node::real, token);
}

AST::Node Lexer::next_operator() {
  int peek = file_.peek();
#ifdef DEBUG
  if (!std::ispunct(peek))
    throw "Non-punct character encountered as first character in next_operator.";
#endif

  // first look for (){}[]
  switch (peek) {
    case static_cast<int>('('):
      {
        file_.get();
        return AST::Node(AST::Node::left_paren);
      }
    case static_cast<int>(')'):
      {
        file_.get();
        return AST::Node(AST::Node::right_paren);
      }
    case static_cast<int>('{'):
      {
        file_.get();
        return AST::Node(AST::Node::left_brace);
      }
    case static_cast<int>('}'):
      {
        file_.get();
        return AST::Node(AST::Node::right_brace);
      }
    case static_cast<int>('['):
      {
        file_.get();
        return AST::Node(AST::Node::left_bracket);
      }
    case static_cast<int>(']'):
      {
        file_.get();
        return AST::Node(AST::Node::right_bracket);
      }
  }

  std::string token;
  do {
    token += static_cast<char>(file_.get());
    peek = file_.peek();
  } while (std::ispunct(peek));

  // It's a line-comment if it has at least two characters and the first two
  // characters are forward slashes
  if (token.size() >= 2 && token[0] == '/' && token[1] == '/') {
    // drop the first two characters.
    token = token.substr(2);
    do {
      token += static_cast<char>(file_.get());
      peek = file_.peek();
    } while (!isnewline(peek));
    return AST::Node(AST::Node::comment, token);
  }

  if (token.size() >= 2 && token[0] == '=' && token[1] == '>') {
    token = token.substr(2);
    return AST::Node(AST::Node::key_value_joiner, "");
  }

  return AST::Node(AST::Node::operat, token);
}
