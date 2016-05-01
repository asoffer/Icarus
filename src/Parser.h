#ifndef ICARUS_PARSER_H
#define ICARUS_PARSER_H

#include <memory>
#include "Lexer.h"

class Parser {
  public:

    Parser(const std::string& filename);

    AST::Node *parse();

  private:
    bool should_shift();
    void shift();
    void ignore();
    bool reduce();
    AST::Node *cleanup();

    void show_debug() const;

    NPtrVec stack_;
    std::unique_ptr<AST::TokenNode> lookahead_;
    Lexer lexer_;
    ParserMode mode_;
};

#endif  // ICARUS_PARSER_H
