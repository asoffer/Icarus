#ifndef ICARUS_LANGUAGE_H
#define ICARUS_LANGUAGE_H

namespace Language {
  // Using masks to make determination node types easier.
  // Starting masks in the 8th bit, leaves bits 0-7 for standard enumeration.
  // This is safe because we will never have more than 128 NodeTypes in a given
  // section.
  constexpr int MASK_binary_operator      = 1 << 8;
  constexpr int MASK_left_unary_operator  = 1 << 9;
  constexpr int MASK_right_unary_operator = 1 << 9;
  constexpr int MASK_expression           = 1 << 10;

  constexpr int MASK_operator =
    MASK_binary_operator | MASK_left_unary_operator | MASK_right_unary_operator;

  enum NodeType {
    unknown, bof, eof, newline, comment,
    identifier,

    // Literals
    int_literal, uint_literal,
    real_literal, char_literal,
    string_literal, type_literal, fn_literal,


    // Operators
    unary_operator,

    key_value_pair, key_value_pair_list,
    comma_list,
    fn_expression, scope,
    void_return_expression,
    DECL_LIST,
    fn_assignment,
    statements, for_statement, while_statement, if_statement, if_else_statement,
    missing_newline_statements,

    // Parens, braces, and brackets
    left_paren, right_paren, left_brace, right_brace, left_bracket, right_bracket,
    semicolon,

    // TODO use RESERVED_MACRO to generate these.
    // Figure out the right way to do that given that they may need different masks
    // Reserved words
    reserved_break, reserved_if, reserved_else, reserved_case, reserved_for, 
    reserved_in, reserved_enum, reserved_while, reserved_continue,
    reserved_ascii, reserved_import, reserved_string, reserved_alloc,
    reserved_struct,

    break_statement, continue_statement,

    STMT_DECL_STD,
    STMT_DECL_INFER,
    STMT_DECL_IN,
    STMT_DECL_GENERATE,

    // BEGIN USING MASKS

    // binary operators
    generic_operator = MASK_binary_operator,

    DECL_OPERATOR_STD,
    DECL_OPERATOR_INFER,
    DECL_OPERATOR_IN,

    assign_operator,
    fn_arrow,
    binary_boolean_operator,
    bool_operator,
    comma,
    dot,
    rocket_operator,

    // left unary operators
    reserved_return = MASK_left_unary_operator,
    reserved_print, reserved_free,
    dereference, 
    dots,

    // unary operator + binary operator
    indirection = MASK_binary_operator | MASK_left_unary_operator,
    negation,

    // expressions
    expression = MASK_expression,
    fn_declaration,
    assignment,
    reserved_true,
    reserved_false,
    reserved_null,
    reserved_type,

    // expression + binary operator
    DECL_OPERATOR_GENERATE = MASK_binary_operator | MASK_expression,
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
constexpr size_t assoc_mask = 3;

namespace Language {
enum class Operator {
#define OPERATOR_MACRO(name, symbol, prec, assoc) name,
#include "config/operator.conf"
#undef OPERATOR_MACRO
};

enum class Terminal {
  ASCII,
  Alloc,
  Return,
  Else,
  True,
  False,
  Null,
  Char,
  Int,
  Real,
  Type,
  UInt,
  StringLiteral
};

inline bool is_expression(NodeType t) { return (t & MASK_expression) != 0; }

inline bool is_binary_operator(NodeType t) {
  return (t & MASK_binary_operator) != 0;
}

inline bool is_operator(NodeType t) { return (t & MASK_operator) != 0; }

inline bool is_decl(NodeType t) {
  return t == DECL_OPERATOR_INFER || t == DECL_OPERATOR_STD ||
         t == DECL_OPERATOR_GENERATE || t == DECL_OPERATOR_IN;
}

extern size_t precedence(Language::Operator op);
extern const std::map<std::string, Language::Operator> lookup_operator;
extern const std::map<std::string, NodeType> reserved_words;
extern const std::map<std::string, size_t> op_prec;
} // namespace Language

#endif // ICARUS_LANGUAGE_H
