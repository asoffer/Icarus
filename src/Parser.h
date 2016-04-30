#ifndef ICARUS_PARSER_H
#define ICARUS_PARSER_H

#include <memory>
#include <iostream>

#include "Lexer.h"
#include "Rule.h"
#include "AST.h"
#include "Language.h"

class Parser {
  public:

    Parser(const std::string& filename);

    AST::Node *parse();

  private:
    bool should_shift();
    void shift();
    bool reduce();
    AST::Node *cleanup();

    void show_debug() const;

    NPtrVec stack_;
    std::unique_ptr<AST::TokenNode> lookahead_;
    Lexer lexer_;

    enum class Mode { Good, BadLine, BadBlock, BadFile, Done } mode_;
};

#endif  // ICARUS_PARSER_H
