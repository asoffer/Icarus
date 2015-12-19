#include "Language.h"

#include "AST.h"

// This is intentionally not accessible in other translation units.
template<size_t N> NPtr drop_all_but(NPtrVec&& nodes) {
  return std::move(nodes[N]);
}

namespace Language {
  const std::map<Language::NodeType, std::string> show_name = {
    { unknown,                 "Unknown" },
    { eof,                     "EOF" },
    { newline,                 "Newline" },
    { comment,                 "Comment" },
    { identifier,              "Identifier" },
    { integer_literal,         "Integer" },
    { real_literal,            "Real" },
    { type_literal,            "Type" },
    { character_literal,       "Character" },
    { string_literal,          "String" },
    { generic_operator,        "Operator" },
    { bool_operator,           "BoolOperator" },
    { binary_boolean_operator, "BinOperator" },
    { decl_operator,           ":" },
    { decl_assign_operator,    ":=" },
    { assign_operator,         "X=" },
    { fn_arrow,                "->" },
    { comma,                   "," },
    { semicolon,               ";" },
    { dereference,             "@" },
    { indirection,             "&" },
    { rocket_operator,         "=>" },
    { key_value_pair,          "( => )" },
    { expression,              "Expression" },
    { left_paren,              "Left Paren" },
    { right_paren,             "Right Paren" },
    { left_brace,              "Left Brace" },
    { right_brace,             "Right Brace" },
    { left_bracket,            "Left Bracket" },
    { right_bracket,           "Right Bracket" },
    { reserved_break,          "Break" },
    { reserved_bool_literal,   "BoolLiteral" },
    { reserved_if,             "If" },
    { reserved_else,           "Else" },
    { reserved_enum,           "Enum" },
    { reserved_case,           "Case" },
    { reserved_loop,           "Loop" },
    { reserved_print,          "Print" },
    { reserved_while,          "While" },
    { reserved_break,          "Break" },
    { reserved_continue,       "Continue" },
    { reserved_return,         "Return" },
    { reserved_type,           "Type" }
  };

  const std::map<std::string, NodeType> reserved_words = {
    { "true",     reserved_bool_literal },
    { "false",    reserved_bool_literal },
    { "break",    reserved_break },
    { "if",       reserved_if },
    { "else",     reserved_else },
    { "enum",     reserved_enum },
    { "case",     reserved_case },
    { "loop",     reserved_loop },
    { "while",    reserved_while },
    { "break",    reserved_break },
    { "continue", reserved_continue },
    { "print",    reserved_print },
    { "return",   reserved_return },
    { "type",     reserved_type }
  };

  constexpr size_t prec_value(size_t n, size_t assoc) { return (n << 2) + assoc; }

  // Associativity stored in the lowest two bits.
  const std::map<std::string, size_t> op_prec = {
    { "return", prec_value(  0,   non_assoc) },
    { "print",  prec_value(  0,   non_assoc) },
    { ",",      prec_value(  1, chain_assoc) },
    { "=>",     prec_value(  2,   non_assoc) }, 
    { "=",      prec_value(  3,   non_assoc) },
    { ":=",     prec_value(  3,   non_assoc) },
    { ":",      prec_value(  4,   non_assoc) },
    { ":>",     prec_value(  5,  left_assoc) },
    { "->",     prec_value(  6, right_assoc) },
    { "|=",     prec_value(  7, right_assoc) },
    { "^=",     prec_value(  8, right_assoc) },
    { "&=",     prec_value(  9, right_assoc) },
    { "+=",     prec_value( 10, right_assoc) },
    { "-=",     prec_value( 10, right_assoc) },
    { "*=",     prec_value( 11, right_assoc) },
    { "/=",     prec_value( 11, right_assoc) },
    { "%=",     prec_value( 11, right_assoc) },
    { "|",      prec_value( 12, right_assoc) },
    { "^",      prec_value( 13, right_assoc) },
    { "&",      prec_value( 14, right_assoc) },
    { "<",      prec_value( 15, chain_assoc) },
    { ">",      prec_value( 15, chain_assoc) },
    { "<=",     prec_value( 15, chain_assoc) },
    { ">=",     prec_value( 15, chain_assoc) },
    { "==",     prec_value( 15, chain_assoc) },
    { "!=",     prec_value( 15, chain_assoc) },
    { "+",      prec_value( 16, right_assoc) },
    { "-",      prec_value( 16, right_assoc) },
    { "*",      prec_value( 17, right_assoc) },
    { "/",      prec_value( 17, right_assoc) },
    { "%",      prec_value( 17, right_assoc) },
    { "@",      prec_value( 18,  left_assoc) },
    { "[]",     prec_value( 19,  left_assoc) },
    { "()",     prec_value( 19,  left_assoc) },
    { ".",      prec_value( 20,  left_assoc) },
    { "MAX",    prec_value(100,   non_assoc) }
  };

  // Here is the definition for all rules in the langugae. For a rule to be
  // applied, the node types on the top of the stack must match those given in
  // the list (second line of each rule). If so, then the function given in the
  // third line of each rule is applied, replacing the matched nodes. Lastly,
  // the new nodes type is set to the given type in the first line.
  const std::vector<Rule> rules = {
    /* Begin literals */
    Rule(expression,
        { reserved_bool_literal },
        AST::Terminal::build_bool_literal),

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
        { fn_expression, left_brace, statements, right_brace },
        AST::FunctionLiteral::build),

    Rule(expression,
        { left_bracket, expression, semicolon, expression, right_bracket },
        AST::ArrayType::build),

    Rule(expression,
        { left_bracket, indirection, semicolon, expression, right_bracket },
        AST::ArrayType::build_unknown),


    Rule(expression,
        { fn_literal },
        drop_all_but<0>),
    /* End literals */


    /* Begin declaration */
    Rule(declaration,
        { identifier, decl_assign_operator, expression },
        AST::Declaration::build_assign),


    Rule(declaration,
        { identifier, decl_operator, expression },
        AST::Declaration::build_decl),

    Rule(fn_declaration,
        { identifier, decl_operator, fn_expression },
        AST::Declaration::build_decl),


    // TODO Should this be an expression or declaration
    Rule(declaration,
        { left_paren, declaration, right_paren },
        AST::Expression::parenthesize),
 
    Rule(expression,
        { left_paren, fn_declaration, right_paren },
        AST::Expression::parenthesize),

    Rule(declaration_comma_list,
        { declaration, comma, declaration },
        AST::ChainOp::build),

    Rule(declaration_comma_list,
        { declaration_comma_list, comma, declaration },
        AST::ChainOp::build),

    Rule(expression,
        { left_paren, declaration_comma_list, right_paren},
        AST::Expression::parenthesize),
    /* End declaration */


    /* Begin assignment */
    Rule(assignment,
        { expression, assign_operator, expression },
        AST::Assignment::build),

    Rule(assignment,
        { declaration, assign_operator, expression },
        AST::Assignment::build),

    Rule(assignment,
        { left_paren, assignment, right_paren},
        AST::Expression::parenthesize),

    Rule(fn_assignment,
        { fn_declaration, assign_operator, expression },
        AST::Assignment::build),

    Rule(fn_assignment,
        { left_paren, fn_assignment, right_paren},
        AST::Expression::parenthesize),

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

    Rule(expression,
        { dereference, expression },
        AST::Unop::build),

    Rule(fn_expression,
        { left_paren, fn_expression, right_paren },
        AST::Expression::parenthesize),

    Rule(expression,
        { expression, generic_operator, expression },
        AST::Binop::build),

    // <expr> & <expr>
    Rule(expression,
        { expression, indirection, expression },
        AST::ChainOp::build),

    // <expr> | <expr>, <expr> ^ <expr>
    Rule(expression,
        { expression, bool_operator, expression },
        AST::ChainOp::build),

    Rule(expression,
        { expression, binary_boolean_operator, expression },
        AST::ChainOp::build),

    Rule(print_expression,
        { reserved_print, expression },
        AST::Unop::build),

    Rule(return_expression,
        { reserved_return, expression },
        AST::Unop::build),

    Rule(expression,
        { fn_expression },
        drop_all_but<0>),

    Rule(fn_expression,
        { expression, fn_arrow, expression },
        AST::Binop::build),

    Rule(fn_expression,
        { declaration, fn_arrow, expression },
        AST::Binop::build),
    /* End expression */


    /* Begin void return */
    Rule(void_return_expression,
        { reserved_return, newline},
        AST::Terminal::build_void_return),
    /* End void return */

    /* Begin paren/bracket operators */
    Rule(expression,
        { expression, left_paren, expression, right_paren },
        AST::Binop::build_paren_operator),

    Rule(expression,
        { expression, left_paren, right_paren },
        AST::Unop::build_paren_operator),

    Rule(expression,
        { expression, left_bracket, expression, right_bracket },
        AST::Binop::build_bracket_operator),

    Rule(expression,
        { left_bracket, expression, right_bracket },
        AST::ArrayLiteral::build),

    /* End paren/bracket operators */

    /* Begin if */
    Rule(if_statement,
        { reserved_if, expression, left_brace, statements, right_brace },
        AST::Conditional::build_if),

    Rule(if_statement,
        { reserved_if, assignment, left_brace, statements, right_brace },
        AST::Conditional::build_if_assignment_error),

    Rule(if_statement,
        { if_statement, reserved_else, if_statement },
        AST::Conditional::build_else_if),

    Rule(if_else_statement,
        { if_statement, reserved_else, left_brace, statements, right_brace },
        AST::Conditional::build_else),

    Rule(if_else_statement,
        { if_else_statement, reserved_else, left_brace, statements, right_brace },
        AST::Conditional::build_extra_else_error),

    Rule(if_else_statement,
        { if_else_statement, reserved_else, if_statement },
        AST::Conditional::build_extra_else_if_error),


    /* End if */

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
        { void_return_expression },
        AST::Statements::build_one),

    Rule(statements,
        { expression, newline },
        AST::Statements::build_one),

    Rule(statements,
        { if_statement, newline },
        AST::Statements::build_one),

    Rule(statements,
        { if_else_statement, newline },
        AST::Statements::build_one),

    Rule(statements,
        { while_statement, newline },
        AST::Statements::build_one),

    Rule(statements,
        { print_expression, newline },
        AST::Statements::build_one),

    Rule(statements,
        { return_expression, newline },
        AST::Statements::build_one),

    Rule(statements,
        { break_statement, newline },
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
        { statements, if_statement, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, if_else_statement, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, while_statement, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, void_return_expression },
        AST::Statements::build_more),

    Rule(statements,
        { statements, print_expression, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, return_expression, newline },
        AST::Statements::build_more),

    Rule(statements,
        { statements, break_statement, newline },
        AST::Statements::build_more),

    Rule(statements,
        { newline, statements },
        drop_all_but<1>),

    Rule(statements,
        { statements, newline },
        drop_all_but<0>),
    /* End statements */

    /* Begin comma list */
    Rule(expression,
        { expression, comma, expression },
        AST::ChainOp::build),
    /* End comma list */

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

    Rule(key_value_pair_list, // An error, they probably meant `==` instead of `=`
        { assignment, rocket_operator, expression, newline },
        AST::KVPairList::build_one_assignment_error),

    Rule(key_value_pair_list, // An error, they probably meant `==` instead of `=`
        { key_value_pair_list, assignment, rocket_operator, expression, newline },
        AST::KVPairList::build_more_assignment_error),

    Rule(expression,
        { reserved_case, left_brace, key_value_pair_list, right_brace },
        AST::Case::build),
    /* End case statements */


    /* Begin while loop */
    Rule(while_statement,
        { reserved_while, expression, left_brace, statements, right_brace },
        AST::While::build),

    Rule(while_statement,
        { reserved_while, assignment, left_brace, statements, right_brace },
        AST::While::build_assignment_error),

    /* End while loop */


    /* Begin loop extras */
    Rule(break_statement,
        { reserved_break },
        AST::Break::build),
    /* End loop extras */


    /* Begin type literals */
    // TODO tighten this up. Just taking in any statements probably captures
    // way too much.
    Rule(expression,
        { reserved_type, left_brace, statements, right_brace },
        AST::TypeLiteral::build),
    /* End type literals */

    /* Begin enums */
    // TODO tighten this up. Just taking in any statements probably captures
    // way too much.
    Rule(expression,
        { reserved_enum, left_brace, statements, right_brace },
        AST::EnumLiteral::build),
    /* End enums */


    /* Begin miscellaneous */
    Rule(newline,
        { newline, newline },
        drop_all_but<0>),

    Rule(left_brace,
        { newline, left_brace },
        drop_all_but<1>),

    Rule(left_brace,
        { left_brace, newline },
        drop_all_but<0>),

    Rule(right_brace,
        { newline, right_brace },
        drop_all_but<1>),

//    Rule(missing_newline_statements,
//        { expression, expression },
//        AST::Statements::build_double_expression_error),
//
//    Rule(missing_newline_statements,
//        { missing_newline_statements, expression },
//        AST::Statements::build_extra_expression_error),
    /* End miscellaneous */ 
  };
}  // namespace Language
