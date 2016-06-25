#ifndef ICARUS_PARSER_H
#define ICARUS_PARSER_H

#ifndef ICARUS_UNITY
#include "Lexer.h"
#include "SourceFile.h"
#endif

class Parser {
public:
  Parser(SourceFile *sf);

  void parse();

private:
  bool should_shift();
  void shift();
  void ignore();
  bool reduce();
  void cleanup();

  void show_debug() const;

  std::vector<Language::NodeType> node_type_stack_;
  NPtrVec node_stack_;
  NNT lookahead_;
  Lexer lexer_;

  SourceFile *source_file_;

  AST::Node *get(size_t n);
  Language::NodeType get_type(size_t n);
};

#endif // ICARUS_PARSER_H
