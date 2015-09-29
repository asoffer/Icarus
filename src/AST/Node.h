#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <string>
#include <iostream>
#include <map>

namespace AST {
  class Node {
    public:
      enum Type {
        unknown, eof, newline, comment,
        identifier,
        integer, real, string_literal,
        generic_operator, declaration, assignment, key_value_joiner,
        key_value_pair, key_value_pair_list,
        expression, paren_expression,
        statements,
        left_paren, right_paren, left_brace, right_brace, left_bracket, right_bracket,
        reserved_if, reserved_else, reserved_case, reserved_loop,
        reserved_while, reserved_break, reserved_continue, reserved_return
      };

      static std::map<Type, std::string> debug_map;

      static inline Node eof_node() { return Node(eof, ""); }
      static inline Node newline_node() { return Node(newline, ""); }
      static inline Node string_literal_node(const std::string& str_lit) { return Node(string_literal, str_lit); }


      inline Type node_type() const { return type_; }
      inline void set_node_type(Type t) { type_ = t; }

      virtual bool is_binop() { return false; }
      virtual void separate_declarations_and_assignments();

      inline std::string token() const { return token_; }

      Node(Type type = unknown, const std::string& token = "");
      virtual ~Node(){}

      virtual std::string to_string(size_t n) const;

      friend std::ostream& operator<<(std::ostream& os, const Node& node);

    protected:
      Type type_;
      std::string token_;
  };

}  // namespace AST

#endif  // ICARUS_AST_NODE_H
