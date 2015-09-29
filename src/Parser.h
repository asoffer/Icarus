#ifndef ICARUS_PARSER_H
#define ICARUS_PARSER_H

#include <memory>
#include <iostream>

#include "Lexer.h"
#include "Rule.h"
#include "AST/Node.h"
#include "typedefs.h"
#include "Language.h"

class Parser {
  public:

    Parser(const char* filename);

    NPtr parse();

  private:
    bool should_shift();
    void shift();
    bool reduce();

    void init_rules();

    std::vector<Rule> rules_;
    std::vector<NPtr> stack_;
    NPtr lookahead_;
    Lexer lexer_;
};

inline void Parser::shift() {
  NPtr next_node_ptr(new AST::Node);
  lexer_ >> *next_node_ptr;

  // Never shift comments onto the stack
  if (next_node_ptr->node_type() == Language::comment) {
    shift();
    return;
  }

  stack_.push_back(std::move(lookahead_));
  lookahead_ = std::move(next_node_ptr);
}

#endif  // ICARUS_PARSER_H
