#ifndef ICARUS_LEXER_H
#define ICARUS_LEXER_H

#include <fstream>
#include <string>
#include "AST.h"

// class Lexer:
//
// This class takes a file name in its only constructor. The contents of the
// file are read character by character and separated into tokens
// according to syntax of Icarus.
class Lexer {
  public:
    explicit Lexer(const std::string& file_name);
 
    ~Lexer();

    // Reads the next token from the lexer into a node
    friend Lexer& operator>>(Lexer& l, AST::TokenNode& node);

    // Returns true precisely when we have not yet reached EOF
    operator bool () const { return !file_.eof() && !file_.fail(); }

    std::string filename() const { return std::string(file_name_); }

  private:
    const char* file_name_;
    std::ifstream file_;
    size_t line_num_;

    Lexer() = delete;

    AST::TokenNode next_word();
    AST::TokenNode next_number();
    AST::TokenNode next_operator();
    AST::TokenNode next_string_literal();
    AST::TokenNode next_char_literal();
    AST::TokenNode next_given_slash();
};

#endif  // ICARUS_LEXER_H
