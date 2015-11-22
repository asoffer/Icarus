#ifndef ICARUS_LANGUAGE_H
#define ICARUS_LANGUAGE_H

#include "Type.h"

namespace Language {
  enum NodeType {
    unknown, eof, newline, comment,
    identifier,

    // Literals
    bool_literal, integer_literal, real_literal, character_literal,
    string_literal, type_literal, fn_literal,

    // Operators
    unary_operator, generic_operator, binary_boolean_operator, decl_operator,
    decl_assign_operator, assign_operator, fn_arrow, rocket_operator,

    key_value_pair, key_value_pair_list,
    comma_list,
    expression, fn_expression, scope, return_expression,
    print_expression, void_return_expression,
    declaration, paren_declaration, fn_declaration,
    assignment, fn_assignment,
    statements, while_statement, if_statement,

    // Parens, braces, and brackets
    left_paren, right_paren, left_brace, right_brace, left_bracket, right_bracket,
    semicolon, comma, dereference,

    // Reserved words
    reserved_bool_literal,
    reserved_if, reserved_else, reserved_case, reserved_loop,
    reserved_print,
    reserved_while, reserved_break, reserved_continue, reserved_return
  };
} // namespace Language

#include <map>
#include <vector>
#include <string>

#include "Rule.h"

// Constants for associativity
constexpr size_t left_assoc = 0;
constexpr size_t right_assoc = 1;
constexpr size_t non_assoc = 2;
constexpr size_t chain_assoc = 3;

namespace Language {
  inline bool is_expression(NodeType t) {
    return t == expression || t == declaration || t == decl_assign_operator
      || t == fn_declaration || t == assignment || t == return_expression;
  }

  inline bool is_binary_operator(NodeType t) {
    return t == generic_operator || t == decl_operator || t == assign_operator
      || t == decl_assign_operator || t == fn_arrow || t == binary_boolean_operator
      || t == comma;
  }

  inline bool is_operator(NodeType t) {
    return is_binary_operator(t) || t == reserved_return || t == reserved_print;
  }

  inline bool is_decl(NodeType t) {
    return t == decl_operator || t == decl_assign_operator;
  }


  extern const std::map<NodeType, std::string> show_name;
  extern const std::map<std::string, NodeType> reserved_words;
  extern const std::map<std::string, size_t> op_prec;

}  // namespace Language

#endif  // ICARUS_LANGUAGE_H
