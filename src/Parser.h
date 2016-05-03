#ifndef ICARUS_PARSER_H
#define ICARUS_PARSER_H

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

    bool reduce_singleton_tokens();

    void show_debug() const;

    NPtrVec stack_;
    AST::Node *lookahead_;
    Lexer lexer_;
    ParserMode mode_;
};

#endif  // ICARUS_PARSER_H
