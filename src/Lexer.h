#ifndef LEXER_H
#define LEXER_H

#include <fstream>
#include "AST/Node.h"

/* class Lexer
 *
 * This class takes a file name in its only constructor. The contents of the
 * file are read character by character and separated into tokens
 * according to syntax of Icarus.
 */
class Lexer {
  public:
    explicit Lexer(const char* file_name);
    ~Lexer();

    friend Lexer& operator>>(Lexer& l, AST::Node& node);

    operator bool () const;

  private:
    const char* file_name_;
    std::ifstream file_;

    Lexer() = delete;

    AST::Node next_word();
    AST::Node next_number();
    AST::Node next_operator();
};

inline Lexer::operator bool () const {
  return !file_.eof() && !file_.fail();
}

#endif
