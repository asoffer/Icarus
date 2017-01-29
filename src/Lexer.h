#ifndef ICARUS_LEXER_H
#define ICARUS_LEXER_H

struct NNT {
  NNT() = default;
  AST::Node *node              = nullptr;
  Language::NodeType node_type = Language::bof;
  NNT(AST::Node *n, Language::NodeType nt) : node(n), node_type(nt) {}
  static NNT Invalid() {
    // Name of this function is clearer than just using default constructor
    return NNT();
  }
};
inline bool operator==(NNT lhs, NNT rhs) {
  return lhs.node == rhs.node && lhs.node_type == rhs.node_type;
}
inline bool operator!=(NNT lhs, NNT rhs) { return (lhs == rhs); }

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

  NNT Next(); // Reads the next token from the lexer into a node

  Cursor cursor;
};

#endif // ICARUS_LEXER_H
