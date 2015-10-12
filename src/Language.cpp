#include "Language.h"

#include "AST.h"

// This is intentionally not accessible in other translation units.
template <size_t N> NPtr drop_all_but(NPtrVec&& nodes) {
  return std::move(nodes[N]);
}

namespace Language {
  const std::map<Language::NodeType, std::string> show_name = {
    { unknown, "Unknown" },
    { eof, "EOF" },
    { newline, "Newline" },
    { comment, "Comment" },
    { identifier, "Identifier" },
    { integer_literal, "Integer" },
    { real_literal, "Real" },
    { type_literal, "Type" },
    { character_literal, "Character" },
    { string_literal, "String" },
    { generic_operator, "Operator" },
    { binary_boolean_operator, "BinOperator" },
    { decl_operator, ":" },
    { assign_operator, "=" },
    { fn_arrow, "->" },
    { rocket_operator, "=>" },
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
    { "return", 1 },
    { "=",  1 },
    { ":=", 2 },
    { ":",  2 },
    { ":>", 2 },
    { "=>", 3 },
    { "->", 3 },
    { "<",  4 },
    { ">",  4 },
    { "<=", 4 },
    { ">=", 4 },
    { "==", 4 },
    { "!=", 4 },
    { "+=", 5 },
    { "-=", 5 },
    { "*=", 6 },
    { "/=", 6 },
    { "%=", 6 },
    { "+",  7 },
    { "-",  7 },
    { "*",  8 },
    { "/",  8 },
    { "%",  8 },
    { "[]", 10 },
    { "()", 10 },
    { "MAX", 1000 }
  };

  const std::vector<Rule> rules = {
    /* Begin literals */
    Rule(expression,
        { identifier },
        AST::Identifier::build),

    Rule(expression,
        { integer_literal },
        AST::Terminal::build_integer_literal),

    Rule(expression,
        { real_literal },
        AST::Terminal::build_real_literal),

    Rule(expression,
        { string_literal },
        AST::Terminal::build_string_literal),

    Rule(expression,
        { type_literal },
        AST::Terminal::build_type_literal),

    Rule(expression,
        { character_literal },
        AST::Terminal::build_character_literal),

    Rule(fn_literal,
        { fn_expression, scope },
        AST::FunctionLiteral::build),

    Rule(expression,
        { fn_literal },
        drop_all_but<0>),
    /* End literals */
    

    /* Begin declaration */
    Rule(declaration,
        { identifier, decl_operator, expression },
        AST::Declaration::build),

    Rule(fn_declaration,
        { identifier, decl_operator, fn_expression },
        AST::Declaration::build),

    // TODO Should this be an expression or declaration
    Rule(expression,
        { left_paren, declaration, right_paren },
        AST::Expression::parenthesize),
    /* End declaration */


    /* Begin assignment */
    Rule(assignment,
        { expression, assign_operator, expression },
        AST::Assignment::build),

    Rule(assignment,
        { declaration, assign_operator, expression },
        AST::Assignment::build),

    Rule(fn_assignment,
        { fn_declaration, assign_operator, expression },
        AST::Assignment::build),

    Rule(assignment,
        { expression, assign_operator, fn_expression },
        AST::Assignment::build),

    Rule(assignment,
        { declaration, assign_operator, fn_expression },
        AST::Assignment::build),

    Rule(fn_assignment,
        { fn_declaration, assign_operator, fn_expression },
        AST::Assignment::build),

    Rule(assignment,
        { expression, assign_operator, fn_literal },
        AST::Assignment::build),

    Rule(assignment,
        { declaration, assign_operator, fn_literal },
        AST::Assignment::build),

    Rule(fn_assignment,
        { fn_declaration, assign_operator, fn_literal },
        AST::Assignment::build),


    /* End assignment */

    
    /* Begin expression */
    Rule(expression,
        { left_paren, expression, right_paren },
        AST::Expression::parenthesize),

    Rule(fn_expression,
        { left_paren, fn_expression, right_paren },
        AST::Expression::parenthesize),

    Rule(expression,
        { expression, generic_operator, expression },
        AST::Binop::build),

    Rule(expression,
        { expression, binary_boolean_operator, expression },
        AST::ChainOp::build),

    Rule(return_expression,
        { reserved_return, expression },
        AST::Unop::build),

    Rule(expression,
        { fn_expression },
        drop_all_but<0>),

    Rule(fn_expression,
        { expression, fn_arrow, expression },
        AST::Binop::build),
    /* End expression */


    /* Begin paren/bracket operators */
    Rule(expression,
        { expression, left_paren, expression, right_paren },
        AST::Binop::build_paren_operator),

    Rule(expression,
        { expression, left_bracket, expression, right_bracket },
        AST::Binop::build_bracket_operator),
    /* End paren/bracket operators */


    /* Begin statements */
    Rule(statements,
        { assignment, newline },
        AST::Statements::build_one),

    Rule(statements,
        { fn_assignment, newline },
        AST::Statements::build_one),

    Rule(statements,
        { declaration, newline },
        AST::Statements::build_one),

    Rule(statements,
        { fn_declaration, newline },
        AST::Statements::build_one),

    Rule(statements,
        { expression, newline },
        AST::Statements::build_one),

    Rule(statements,
        { return_expression, newline },
        AST::Statements::build_one),

    Rule(statements,
        { scope, newline },
        AST::Statements::build_one),

    Rule(statements,
        { statements, expression, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, assignment, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, fn_assignment, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, declaration, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, fn_declaration, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, return_expression, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, scope, newline },
        AST::Statements::build_more),

    Rule(statements,
        { newline, statements },
        drop_all_but<1>),

    Rule(statements,
        { statements, newline },
        drop_all_but<0>),
    /* End statements */


    /* Begin case statements */
    Rule(key_value_pair_list,
        { expression, rocket_operator, expression, newline },
        AST::KVPairList::build_one),

    Rule(key_value_pair_list,
        { key_value_pair_list, expression, rocket_operator, expression, newline },
        AST::KVPairList::build_more),

    Rule(key_value_pair_list,
        { reserved_else, rocket_operator, expression, newline },
        AST::KVPairList::build_one),

    Rule(key_value_pair_list,
        { key_value_pair_list, reserved_else, rocket_operator, expression, newline },
        AST::KVPairList::build_more),

    Rule(key_value_pair_list,
        { assignment, rocket_operator, expression, newline },
        AST::KVPairList::build_one_assignment_error),

    Rule(key_value_pair_list,
        { assignment, expression, rocket_operator, expression, newline },
        AST::KVPairList::build_more_assignment_error),


    Rule(key_value_pair_list,
        { key_value_pair_list, newline },
        drop_all_but<0>),

    Rule(expression,
        { reserved_case, left_brace, newline, key_value_pair_list, right_brace },
        AST::Case::build),
    /* End case statements */


    /* Begin miscellaneous */
    Rule(newline,
        { newline, newline },
        drop_all_but<0>),

    Rule(scope,
        { left_brace, statements, right_brace },
        AST::AnonymousScope::build),
    /* End miscellaneous */ 
  };
}  // namespace Language
