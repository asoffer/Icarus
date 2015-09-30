#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <string>
#include <iostream>
#include "Language.h"

namespace AST {
  class Scope;

  class Node {
    public:
      static inline Node eof_node() {
        return Node(Language::eof, "");
      }
      static inline Node newline_node() {
        return Node(Language::newline, "");
      }
      static inline Node string_literal_node(const std::string& str_lit) { 
        return Node(Language::string_literal, str_lit);
      }

      Language::NodeType node_type() const { return type_; }
      void set_node_type(Language::NodeType t) { type_ = t; }

      std::string token() const { return token_; }
      void set_token(const std::string& token_string) {
        token_ = token_string;
      }


      virtual void join_identifiers(Scope*) {};
      virtual bool is_identifier() const { return false; }
      virtual bool is_binop() const { return false; }


      Node(Language::NodeType type = Language::unknown,
          const std::string& token = "");

      virtual ~Node(){}


      virtual std::string to_string(size_t n) const;
      friend std::ostream& operator<<(std::ostream& os, const Node& node);

    protected:
      Language::NodeType type_;
      std::string token_;
  };

}  // namespace AST

#endif  // ICARUS_AST_NODE_H
