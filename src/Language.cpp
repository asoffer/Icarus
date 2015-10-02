#include "Language.h"

#include "AST/Node.h"
#include "AST/Expression.h"
#include "AST/Terminal.h"
#include "AST/Identifier.h"
#include "AST/Binop.h"
#include "AST/Declaration.h"
#include "AST/Assignment.h"
#include "AST/KVPairList.h"
#include "AST/Case.h"
#include "AST/Scope.h"
#include "AST/AnonymousScope.h"
#include "AST/Statements.h"

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
    { integer, "Integer" },
    { real, "Real" },
    { type_literal, "Type" },
    { string_literal, "String" },
    { generic_operator, "Operator" },
    { decl_operator, ":" },
    { assign_operator, "=" },
    { fat_arrow, "=>" },
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


  const std::map<std::string, AST::Type> type_literals = {
    { "bool",   AST::t_bool },
    { "char",   AST::t_char },
    { "int",    AST::t_int },
    { "real",   AST::t_real },
    { "string", AST::t_string }
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
        { integer },
        AST::Terminal::build_integer),

    Rule(expression,
        { real },
        AST::Terminal::build_real),

    Rule(expression,
        { string_literal },
        AST::Terminal::build_string_literal),

    Rule(expression,
        { type_literal },
        AST::Terminal::build_type_literal),
    /* End literals */
    

    /* Begin declaration */
    Rule(declaration,
        { identifier, decl_operator, expression },
        AST::Declaration::build),

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
    /* End assignment */


    /* Begin expression */
    Rule(expression,
        { left_paren, expression, right_paren },
        AST::Expression::parenthesize),

    Rule(expression,
        { expression, generic_operator, expression },
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


    /* Begin return statements */
    Rule(statements,
        { reserved_return, expression },
        AST::Statements::build_one),
    /* End return statements */


    /* Begin statements */
    Rule(statements,
        { assignment, newline },
        AST::Statements::build_one),

    Rule(statements,
        { declaration, newline },
        AST::Statements::build_one),

    Rule(statements,
        { expression, newline },
        AST::Statements::build_one),

    Rule(statements,
        { statements, expression, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, assignment, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, declaration, newline },
        AST::Statements::build_more),

    Rule(statements,
        { newline, statements },
        drop_all_but<1>),

    Rule(statements,
        { statements, newline },
        drop_all_but<0>),
    /* End statements */


    /* Begin case statements */
    Rule(key_value_pair,
        { expression, fat_arrow, expression, newline },
        AST::Binop::build),

    Rule(key_value_pair,
        { reserved_else, fat_arrow, expression, newline },
        AST::Binop::build_else_kv),

    Rule(key_value_pair,
        { key_value_pair, newline },
        drop_all_but<0>),

    Rule(key_value_pair_list,
        { key_value_pair },
        AST::KVPairList::build_one),

    Rule(key_value_pair_list,
        { key_value_pair_list, newline },
        drop_all_but<0>),

    Rule(key_value_pair_list,
        { key_value_pair_list, key_value_pair },
        AST::KVPairList::build_more),

    Rule(expression,
        { reserved_case, left_brace, newline, key_value_pair_list, right_brace },
        AST::Case::build),
    /* End case statements */


    /* Begin miscellaneous */
    Rule(newline,
        { newline, newline },
        drop_all_but<0>),

    Rule(expression,
        { left_brace, statements, right_brace },
        AST::AnonymousScope::build),
    /* End miscellaneous */ 
  };
}  // namespace Language
