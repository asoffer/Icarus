#include "Lexer.h"

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

  return AST::Node(AST::Node::identifier, token);
}

AST::Node Lexer::next_number() {
#ifdef DEBUG
  if (!std::isdigit(file_.peek()))
    throw "Non-digit character encountered as first character in next_word.";
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
  // TODO(andy) fill out this stub
  file_.get();
  return AST::Node(AST::Node::operat, "??");
}
