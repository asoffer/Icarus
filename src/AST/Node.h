#ifndef AST_NODE_H
#define AST_NODE_H

#include <string>
#include <iostream>

namespace AST {
  class Node {
    public:
      enum Type {
        unknown, eof, newline, comment,
        identifier,
        integer, real,
        operat,
        left_paren, right_paren, left_brace, right_brace, left_bracket, right_bracket,
        reserved_if, reserved_else, reserved_case, reserved_loop,
        reserved_while, reserved_break, reserved_continue, reserved_return
      };

      static Node eof_node();
      static Node newline_node();

      Node(Type type = unknown, const std::string& token = "");

#ifdef DEBUG
    friend std::ostream& operator<<(std::ostream& os, const Node& node);
#endif

    private:
      Type type_;
      std::string token_;
  };

  inline Node Node::eof_node() {
    return Node(eof, "");
  }

  inline Node Node::newline_node() {
    return Node(newline, "");
  }

}  // namespace AST

#endif  // AST_NODE_H
