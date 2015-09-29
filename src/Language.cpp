#include "Language.h"

namespace Language {
  const std::map<Language::NodeType, std::string> show_name = {
    { unknown, "Unknown" },
    { eof, "EOF" },
    { newline, "Newline" },
    { comment, "Comment" },
    { identifier, "Identifier" },
    { integer, "Integer" },
    { real, "Real" },
    { string_literal, "String" },
    { generic_operator, "Operator" },
    { declaration, ":" },
    { assignment, "=" },
    { key_value_joiner, "=>" },
    { key_value_pair, "( => )" },
    { expression, "Expression" },
    { left_paren, "Left Paren" },
    { right_paren, "Right Paren" },
    { left_brace, "Left Brace" },
    { right_brace, "Right Brace" },
    { left_bracket, "Left Bracket" },
    { right_bracket, "Right Bracket" },
    { reserved_if, "If" },
    { reserved_else, "Else" },
    { reserved_case, "Case" },
    { reserved_loop, "Loop" },
    { reserved_while, "While" },
    { reserved_break, "Break" },
    { reserved_continue, "Continue" },
    { reserved_return, "Return" }
  };

  const std::map<std::string, NodeType> reserved_words = {
    { "if",       reserved_if },
    { "else",     reserved_else },
    { "case",     reserved_case },
    { "loop",     reserved_loop },
    { "while",    reserved_while },
    { "break",    reserved_break },
    { "continue", reserved_continue },
    { "return",   reserved_return }
  };

  const std::map<std::string, size_t> op_prec = {
    { "=",  1 },
    { ":=", 2 },
    { ":",  2 },
    { "=>", 3 },
    { "->", 3 },
    { "<",  4 },
    { ">",  4 },
    { "<=", 4 },
    { ">=", 4 },
    { "==", 4 },
    { "!=", 4 },
    { "+",  5 },
    { "-",  5 },
    { "*",  6 },
    { "/",  6 },
    { "%",  6 },
    { "[]", 10 },
    { "()", 10 },
    { "MAX", 1000 }
  };

}  // namespace Language
