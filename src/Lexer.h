#ifndef ICARUS_LEXER_H
#define ICARUS_LEXER_H

#include "SourceFile.h"

struct NNT {
  AST::Node *node;
  Language::NodeType node_type;
  NNT(AST::Node *n = nullptr, Language::NodeType nt = Language::unknown)
      : node(n), node_type(nt) {}
};
// struct Lexer:
//
// This class takes a file name in its only constructor. The contents of the
// file are read character by character and separated into tokens
// according to syntax of Icarus.
struct Lexer {
public:
  explicit Lexer(SourceFile *source_file);

  ~Lexer();

  // Reads the next token from the lexer into a node
  NNT Next();

  // TODO is a token location just a cursor?
  struct Cursor {
    Cursor() : offset_(0), line_num_(0), file_name_(nullptr) {}

    pstr line_;
    size_t offset_;
    size_t line_num_;
    const char *file_name_;

    // Get the character that the cursor is currently pointing to
    inline char &operator*(void) const { return *(line_.ptr + offset_); }

    inline TokenLocation Location() {
      TokenLocation result;
      result.line_num = line_num_;
      result.offset   = offset_;
      result.file     = file_name_;
      return result;
    }

  } cursor;

  std::ifstream ifs;

  SourceFile *source_file_;

  Lexer() = delete;

  void IncrementCursor();
  void SkipToEndOfLine();

private:
  NNT NextWord();
  NNT NextNumber();
  NNT NextOperator();
};

#endif // ICARUS_LEXER_H
