#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <string>
#include <iostream>
#include <map>
#include "Language.h"

namespace AST {
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

      inline Language::NodeType node_type() const { return type_; }
      inline void set_node_type(Language::NodeType t) { type_ = t; }

      virtual bool is_binop() { return false; }
      virtual void separate_declarations_and_assignments();

      inline std::string token() const { return token_; }

      Node(Language::NodeType type = Language::unknown, const std::string& token = "");
      virtual ~Node(){}

      virtual std::string to_string(size_t n) const;

      friend std::ostream& operator<<(std::ostream& os, const Node& node);

    protected:
      Language::NodeType type_;
      std::string token_;
  };

}  // namespace AST

#endif  // ICARUS_AST_NODE_H
