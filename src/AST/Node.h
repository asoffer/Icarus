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
        expression,
        left_paren, right_paren, left_brace, right_brace, left_bracket, right_bracket,
        reserved_if, reserved_else, reserved_case, reserved_loop,
        reserved_while, reserved_break, reserved_continue, reserved_return
      };

      static inline Node eof_node() { return Node(eof, ""); }
      static inline Node newline_node() { return Node(newline, ""); }

      inline Type node_type() const { return type_; }

      Node(Type type = unknown, const std::string& token = "");

#ifdef DEBUG
      friend std::ostream& operator<<(std::ostream& os, const Node& node);
#endif

    private:
      Type type_;
      std::string token_;
  };

}  // namespace AST

#endif  // AST_NODE_H
