#ifndef ICARUS_LANGUAGE_H
#define ICARUS_LANGUAGE_H

#include <map>
#include <string>

namespace Language {
  enum NodeType {
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

  extern const std::map<NodeType, std::string> show_name;
  extern const std::map<std::string, NodeType> reserved_words;
  extern const std::map<std::string, size_t> op_prec;
}  // namespace Language

#endif  // ICARUS_LANGUAGE_H
