#ifndef ICARUS_LEXER_H
#define ICARUS_LEXER_H

#include "util/pstr.h"

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
    explicit Lexer(const std::string& file_name);
 
    ~Lexer();

    // Reads the next token from the lexer into a node
    NNT Next();

    // Returns true precisely when we have not yet reached EOF
    operator bool () const { return !ifs.eof() && !ifs.fail(); }

    std::ifstream ifs;
    TokenLocation loc_;

    struct Cursor {
      Cursor() : offset_(0), line_num_(0) {}

      pstr line_;
      size_t offset_;
      size_t line_num_;

      // Get the character that the cursor is currently pointing to
      inline char &operator*(void) const { return *(line_.ptr + offset_); }

    } cursor;

    std::vector<pstr> lines;

    Lexer() = delete;

    void IncrementCursor();

    NNT next_operator();
    NNT next_string_literal();
    NNT next_char_literal();
    NNT next_given_slash();
    NNT next_hashtag();

  private:
    NNT NextWord();
    NNT NextNumber();
};

#endif  // ICARUS_LEXER_H
