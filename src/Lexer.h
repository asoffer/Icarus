#ifndef ICARUS_LEXER_H
#define ICARUS_LEXER_H

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
    operator bool () const { return !file_.eof() && !file_.fail(); }

    std::ifstream file_;
    TokenLocation loc_;

    Lexer() = delete;

    int GetChar();

    NNT next_word();
    NNT next_number();
    NNT next_operator();
    NNT next_string_literal();
    NNT next_char_literal();
    NNT next_given_slash();
    NNT next_hashtag();
};

#endif  // ICARUS_LEXER_H
