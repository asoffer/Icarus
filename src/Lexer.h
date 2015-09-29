#ifndef ICARUS_LEXER_H
#define ICARUS_LEXER_H

#include <fstream>
#include <string>
#include "AST/Node.h"

// class Lexer:
//
// This class takes a file name in its only constructor. The contents of the
// file are read character by character and separated into tokens
// according to syntax of Icarus.
class Lexer {
  public:
    explicit Lexer(const std::string& file_name);
    explicit Lexer(const char* file_name);
 
    ~Lexer();

    // Reads the next token from the lexer into a node
    friend Lexer& operator>>(Lexer& l, AST::Node& node);

    // Returns true precisely when we have not yet reached EOF
    operator bool () const { return !file_.eof() && !file_.fail(); }

  private:
    const char* file_name_;
    std::ifstream file_;

    Lexer() = delete;

    AST::Node next_word();
    AST::Node next_number();
    AST::Node next_operator();
    AST::Node next_string_literal();
};

#endif  // ICARUS_LEXER_H
