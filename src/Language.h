#ifndef ICARUS_LANGUAGE_H
#define ICARUS_LANGUAGE_H

#include "Type.h"

namespace Language {
  enum NodeType {
    unknown, eof, newline, comment,
    identifier,

    // Literals
    integer_literal, real_literal, character_literal, string_literal,
    type_literal, fn_literal,

    // Operators
    unary_operator, generic_operator, binary_boolean_operator, decl_operator,
    assign_operator, fn_arrow, rocket_operator,

    key_value_pair, key_value_pair_list,
    expression, paren_expression, fn_expression, scope, return_expression,
    declaration, paren_declaration, fn_declaration,
    assignment, fn_assignment,
    statements,

    // Parens, braces, and brackets
    left_paren, right_paren, left_brace, right_brace, left_bracket, right_bracket,

    // Reserved words
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
