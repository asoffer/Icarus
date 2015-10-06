#ifndef ICARUS_LANGUAGE_H
#define ICARUS_LANGUAGE_H

#include "Type.h"

namespace Language {
  enum NodeType {
    unknown, eof, newline, comment,
    identifier,
    integer, real, string_literal, type_literal, fn_literal, // 9
    generic_operator, decl_operator, assign_operator, fn_arrow, rocket_operator, // 14
    key_value_pair, key_value_pair_list,
    expression, paren_expression, fn_expression, scope,  // 20
    declaration, paren_declaration, fn_declaration,
    assignment, fn_assignment,
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
    return t == expression || t == declaration
      || t == fn_declaration || t == assignment;
  }
  inline bool is_operator(NodeType t) {
    return t == generic_operator || t == decl_operator
      || t == assign_operator || t == fn_arrow;
  }

  extern const std::map<NodeType, std::string> show_name;
  extern const std::map<std::string, NodeType> reserved_words;
  extern const std::map<std::string, size_t> op_prec;

}  // namespace Language

#endif  // ICARUS_LANGUAGE_H
