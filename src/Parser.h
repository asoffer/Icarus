#ifndef ICARUS_PARSER_H
#define ICARUS_PARSER_H

#ifndef ICARUS_UNITY
#include "Lexer.h"
#endif

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

    std::vector<Language::NodeType> node_type_stack_;
    NPtrVec node_stack_;
    NNT lookahead_;
    Lexer lexer_;

    AST::Node *get(size_t n);
    Language::NodeType get_type(size_t n);
};

#endif  // ICARUS_PARSER_H
