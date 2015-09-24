#ifndef ICARUS_PARSER_H
#define ICARUS_PARSER_H

#include <memory>
#include <iostream>

#include "Lexer.h"
#include "Rule.h"
#include "AST/Node.h"

class Parser {
  public:
    typedef std::unique_ptr<AST::Node> NPtr;

    Parser(const char* filename);

    void parse();

  private:
    void shift();
    bool reduce();

    std::vector<Rule> rules_;
    std::vector<NPtr> stack_;
    Lexer lexer_;
};

inline void Parser::shift() {
  NPtr next_node_ptr(new AST::Node);
  lexer_ >> *next_node_ptr;

  std::cout << *next_node_ptr << std::endl;

  stack_.push_back(std::move(next_node_ptr));
}

#endif  // ICARUS_PARSER_H
