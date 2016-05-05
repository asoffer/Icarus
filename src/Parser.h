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

    void show_debug() const;

    NPtrVec stack_;
    AST::Node *lookahead_;
    Lexer lexer_;
    ParserMode mode_;

    AST::Node *get(size_t n);
    Language::NodeType get_type(size_t n);
};

#endif  // ICARUS_PARSER_H
