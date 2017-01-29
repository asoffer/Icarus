#ifndef ICARUS_LEXER_H
#define ICARUS_LEXER_H

struct NNT {
  AST::Node *node;
  Language::NodeType node_type;
  NNT(AST::Node *n, Language::NodeType nt) : node(n), node_type(nt) {}
};
// struct Lexer:
//
// This class takes a file name in its only constructor. The contents of the
// file are read character by character and separated into tokens
// according to syntax of Icarus.
struct Lexer {
public:
  explicit Lexer(SourceFile *source_file);

  Lexer(const Lexer &) = delete;
  Lexer() = delete;
  ~Lexer();

  NNT Next(); // Reads the next token from the lexer into a node

  Cursor cursor;
  std::ifstream ifs;

private:
  void IncrementCursor();
  void BackUpCursor();
  void MoveCursorToNextLine();
  void SkipToEndOfLine();

  NNT NextWord();
  NNT NextNumber();
  NNT NextOperator();
};

#endif // ICARUS_LEXER_H
