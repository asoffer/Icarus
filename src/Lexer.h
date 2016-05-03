#ifndef ICARUS_LEXER_H
#define ICARUS_LEXER_H

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
    AST::Node *Next();

    // Returns true precisely when we have not yet reached EOF
    operator bool () const { return !file_.eof() && !file_.fail(); }

  private:
    std::ifstream file_;
    TokenLocation loc_;

    Lexer() = delete;

    AST::Node *next_word();
    AST::Node *next_number();
    AST::Node *next_operator();
    AST::Node *next_string_literal();
    AST::Node *next_char_literal();
    AST::Node *next_given_slash();
    AST::Node *next_hashtag();
};

#endif  // ICARUS_LEXER_H
