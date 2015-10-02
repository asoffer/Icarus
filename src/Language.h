#ifndef ICARUS_LANGUAGE_H
#define ICARUS_LANGUAGE_H

#include "AST/Type.h"

namespace Language {
  enum NodeType {
    unknown, eof, newline, comment,
    identifier,
    integer, real, string_literal, type_literal,
    generic_operator, decl_operator, assign_operator, fat_arrow,
    key_value_pair, key_value_pair_list,
    expression, paren_expression,
    declaration, paren_declaration, assignment,
    statements,
    left_paren, right_paren, left_brace, right_brace, left_bracket, right_bracket,
    reserved_if, reserved_else, reserved_case, reserved_loop,
    reserved_while, reserved_break, reserved_continue, reserved_return
  };
} // namespace Language

#include <map>
#include <vector>
#include <string>

#include "Rule.h"

namespace Language {
  inline bool is_expression(NodeType t) {
    return t == expression || t == declaration || t == assignment;
  }
  inline bool is_operator(NodeType t) {
    return t == generic_operator || t == decl_operator|| t == assign_operator;
  }


  // kloodge: int is really AST::Type, but circular include dependencies :(. FIXME
  extern const std::map<std::string, AST::Type> type_literals;
  extern const std::map<NodeType, std::string> show_name;
  extern const std::map<std::string, NodeType> reserved_words;
  extern const std::map<std::string, size_t> op_prec;

}  // namespace Language

#endif  // ICARUS_LANGUAGE_H
