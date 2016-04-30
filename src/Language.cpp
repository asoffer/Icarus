#include "Language.h"
#include "AST.h"

#include <queue>

extern std::queue<std::string> file_queue;
// This is intentionally not accessible in other translation units.
template <size_t N> AST::Node *drop_all_but(NPtrVec &&nodes) {
  return steal<AST::Node>(nodes[N]);
}

// TODO we can't have a '/' character, and since all our programs are in the
// programs/ directory for now, we hard-code that. This needs to be removed.
AST::Node *import_file(NPtrVec &&nodes) {
  file_queue.emplace("programs/" + nodes[1]->token());
  auto tk_node = new AST::TokenNode;
  *tk_node = AST::TokenNode::newline();
  return tk_node;
}

namespace Language {
const std::map<std::string, NodeType> reserved_words = {
#define RESERVED_MACRO(res) { #res, reserved_##res },
#include "config/reserved.conf"
#undef RESERVED_MACRO
};

// Associativity stored in the lowest two bits.
size_t precedence(Operator op) {
  switch (op) {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  case Operator::name:                                                         \
    return (((prec) << 2) + (assoc));
#include "config/operator.conf"
#undef OPERATOR_MACRO
  }
}

const std::map<std::string, Operator> lookup_operator = {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  { #symbol, Operator::name }                                                  \
  ,
#include "config/operator.conf"
#undef OPERATOR_MACRO
};

#define ARGS DECL_LIST, STMT_DECL_STD, STMT_DECL_INFER
// This doesn't allow include 'void' or other things like that.

#define STMT                                                                   \
  STMT_DECL_STD, STMT_DECL_INFER, STMT_IF, STMT_IF_ELSE, STMT_FOR, STMT_WHILE, \
      STMT_JUMP, STMT_ASSIGN

// Here is the definition for all rules in the langugae. For a rule to be
// applied, the node types on the top of the stack must match those given in the
// list (second line of each rule). If so, then the function given in the third
// line of each rule is applied, replacing the matched nodes. Lastly, the new
// nodes type is set to the given type in the first line.
const std::vector<Rule> rules = {
    /* Begin literals */
    Rule(expression, {{reserved_true}}, AST::Terminal::build_true),
    Rule(expression, {{reserved_false}}, AST::Terminal::build_false),
    Rule(expression, {{reserved_null}}, AST::Terminal::build_null),
    Rule(expression, {{identifier}}, AST::Identifier::build),
    Rule(expression, {{uint_literal}}, AST::Terminal::build_uint_literal),
    Rule(expression, {{int_literal}}, AST::Terminal::build_int_literal),
    Rule(expression, {{real_literal}}, AST::Terminal::build_real_literal),
    Rule(expression, {{string_literal}}, AST::Terminal::build_string_literal),
    Rule(expression, {{char_literal}}, AST::Terminal::build_char_literal),
    Rule(expression, {{reserved_input}}, AST::Terminal::build_input),
    Rule(expression, {{reserved_ord}}, AST::Terminal::build_ord),
    Rule(expression, {{reserved_ascii}}, AST::Terminal::build_ASCII),
    Rule(expression, {{reserved_alloc}}, AST::Terminal::build_alloc),
    Rule(expression, {{type_literal}}, AST::Terminal::build_type_literal),

    Rule(expression, {{fn_literal, fn_expression}}, drop_all_but<0>),

    Rule(fn_literal,
         {{fn_expression}, {left_brace}, {statements}, {right_brace}},
         AST::FunctionLiteral::build),

    // TODO rename this type. could be an array type or an array expression
    // depending on the context
    Rule(expression, {{left_bracket},
                      {expression},
                      {semicolon},
                      {expression},
                      {right_bracket}},
         AST::ArrayType::build),

    // TODO make this the correct thing
    Rule(expression, {{left_bracket},
                      {negation},
                      {semicolon},
                      {expression},
                      {right_bracket}},
         AST::ArrayType::build_unknown),
    /* End literals */

    /* Begin declaration */
    Rule(STMT_DECL_STD, {{STMT_DECL_STD}, {hashtag}},
         AST::Declaration::AddHashtag),

    Rule(STMT_DECL_STD, {{identifier},
                         {DECL_OPERATOR_STD},
                         {STMT_DECL_GENERATE, expression, fn_expression}},
         AST::Declaration::BuildStd),

    Rule(STMT_DECL_INFER,
         {{identifier}, {DECL_OPERATOR_INFER}, {expression, fn_expression}},
         AST::Declaration::BuildInfer),
    Rule(DECL_IN, {{identifier}, {reserved_in}, {expression}},
         AST::Declaration::BuildIn),
    Rule(STMT_DECL_GENERATE,
         {{identifier, expression},
          {DECL_OPERATOR_GENERATE},
          {identifier}}, // TODO Should idenifier be a first option?
         AST::Declaration::BuildGenerate),
/* End declaration */

    /* Begin parentheses */
#define PAREN_RULE( node_type ) \
    Rule(node_type, { {left_paren}, {node_type}, {right_paren} }, AST::Expression::parenthesize)

    PAREN_RULE(expression), PAREN_RULE(STMT_ASSIGN), PAREN_RULE(fn_expression),
    PAREN_RULE(STMT_DECL_STD), PAREN_RULE(STMT_DECL_INFER),
    PAREN_RULE(DECL_LIST),

#undef PAREN_RULE
    /* End parentheses */

    /* Begin declaration list */
    // TODO would this include ((a: int, b: int), c: int) and is that what we
    // want?
    Rule(DECL_LIST, {{ARGS}, {comma}, {STMT_DECL_STD, STMT_DECL_INFER}},
         AST::ChainOp::build),
    Rule(DECL_IN_LIST, {{DECL_IN, DECL_IN_LIST}, {comma}, {DECL_IN}},
         AST::ChainOp::build),
    /* End declaration list */

    /* Begin assignment */
    Rule(STMT_ASSIGN, {{STMT_DECL_STD, expression},
                       {assign_operator},
                       {expression, fn_expression, fn_literal}},
         AST::Binop::build_assignment),
    /* End assignment */

    /* Begin expression */
    Rule(expression, {{not_operator, dereference, negation, indirection,
                       reserved_print, reserved_return, reserved_free},
                      {expression}},
         AST::Unop::build),

    Rule(expression, {{expression}, {dots}}, AST::Unop::build_dots),

    Rule(expression,
         {{expression}, {generic_operator, dots, negation}, {expression}},
         AST::Binop::build),

    Rule(expression, {{expression}, {dot}, {identifier}}, AST::Access::build),

    Rule(expression, {{expression},
                      {indirection, bool_operator, binary_boolean_operator},
                      {expression}},
         AST::ChainOp::build),

    Rule(fn_expression, {{expression, ARGS}, {fn_arrow}, {expression}},
         AST::Binop::build),
    /* End expression */

    /* Begin paren/bracket operators */
    Rule(expression, {{expression}, {left_paren}, {expression}, {right_paren}},
         AST::Binop::build_paren_operator),

    Rule(expression, {{expression}, {left_paren}, {right_paren}},
         AST::Unop::build_paren_operator),

    Rule(expression,
         {{expression}, {left_bracket}, {expression}, {right_bracket}},
         AST::Binop::build_bracket_operator),

    Rule(expression, {{left_bracket}, {expression}, {right_bracket}},
         AST::ArrayLiteral::build),
    /* End paren/bracket operators */

    /* Begin if */
    Rule(STMT_IF, {{reserved_if},
                   {expression},
                   {left_brace},
                   {statements},
                   {right_brace}},
         AST::Conditional::build_if),
    Rule(STMT_IF, {{reserved_if},
                   {STMT_ASSIGN},
                   {left_brace},
                   {statements},
                   {right_brace}},
         AST::Conditional::build_if_assignment_error),
    Rule(STMT_IF, {{STMT_IF}, {reserved_else}, {STMT_IF}},
         AST::Conditional::build_else_if),
    Rule(
        STMT_IF_ELSE,
        {{STMT_IF}, {reserved_else}, {left_brace}, {statements}, {right_brace}},
        AST::Conditional::build_else),
    Rule(STMT_IF_ELSE, {{STMT_IF_ELSE},
                        {reserved_else},
                        {left_brace},
                        {statements},
                        {right_brace}},
         AST::Conditional::build_extra_else_error),
    Rule(STMT_IF_ELSE, {{STMT_IF_ELSE}, {reserved_else}, {STMT_IF}},
         AST::Conditional::build_extra_else_if_error),
    /* End if */

    /* Begin statements */
    Rule(statements, {{STMT, expression}, {newline}},
         AST::Statements::build_one),
    Rule(statements, {{statements}, {STMT, expression}, {newline}},
         AST::Statements::build_more),

    Rule(statements, {{statements}, {newline}}, drop_all_but<0>),
    Rule(statements, {{newline}, {statements}}, drop_all_but<1>),
    /* End statements */

    /* Begin comma list */
    // TODO is this even used?
    Rule(expression, {{expression}, {comma}, {expression}},
         AST::ChainOp::build),
    /* End comma list */

    /* Begin case statements */
    Rule(key_value_pair_list,
         {{expression}, {rocket_operator}, {expression}, {newline}},
         AST::KVPairList::build_one),

    Rule(key_value_pair_list, {{key_value_pair_list},
                               {expression},
                               {rocket_operator},
                               {expression},
                               {newline}},
         AST::KVPairList::build_more),

    Rule(key_value_pair_list,
         {{reserved_else}, {rocket_operator}, {expression}, {newline}},
         AST::KVPairList::build_one),

    Rule(key_value_pair_list, {{key_value_pair_list},
                               {reserved_else},
                               {rocket_operator},
                               {expression},
                               {newline}},
         AST::KVPairList::build_more),

    Rule(key_value_pair_list, // An error, they probably meant `==` instead of
                              // `=`
         {{STMT_ASSIGN}, {rocket_operator}, {expression}, {newline}},
         AST::KVPairList::build_one_assignment_error),

    Rule(key_value_pair_list, // An error, they probably meant `==` instead of
                              // `=`
         {{key_value_pair_list},
          {STMT_ASSIGN},
          {rocket_operator},
          {expression},
          {newline}},
         AST::KVPairList::build_more_assignment_error),

    Rule(expression,
         {{reserved_case}, {left_brace}, {key_value_pair_list}, {right_brace}},
         AST::Case::build),
    /* End case statements */

    /* Begin while loop */
    Rule(STMT_WHILE, {{reserved_while},
                      {expression},
                      {left_brace},
                      {statements},
                      {right_brace}},
         AST::While::build),
    Rule(STMT_WHILE, {{reserved_while},
                      {STMT_ASSIGN},
                      {left_brace},
                      {statements},
                      {right_brace}},
         AST::While::build_assignment_error),
    /* End while loop */

    /* Begin for loop */
    Rule(STMT_FOR, {{reserved_for},
                    {DECL_IN, DECL_IN_LIST},
                    {left_brace},
                    {statements},
                    {right_brace}},
         AST::For::build),
    /* End for loop */

    /* Begin loop extras */
    Rule(STMT_JUMP, {{reserved_restart, reserved_break, reserved_repeat,
                      reserved_continue, reserved_return}},
         AST::Jump::build),
    /* End loop extras */

    /* Begin structs and enums */
    // TODO tighten this up. Just taking in any statements probably captures way
    // too much.
    Rule(expression,
         {{reserved_struct}, {left_brace}, {statements}, {right_brace}},
         AST::StructLiteral::build),
    Rule(expression,
         {{reserved_struct}, {ARGS}, {left_brace}, {statements}, {right_brace}},
         AST::StructLiteral::build_parametric),
    Rule(expression,
         {{reserved_enum}, {left_brace}, {statements}, {right_brace}},
         AST::EnumLiteral::build),
    /* End structs and enums */

    /* Begin import */
    Rule(newline, {{reserved_import}, {string_literal}, {newline}},
         import_file),
    /* End import */

    /* Begin miscellaneous */
    Rule(comma, {{comma}, {newline}}, drop_all_but<0>),
    Rule(newline, {{newline}, {newline}}, drop_all_but<0>),
    Rule(left_brace, {{newline}, {left_brace}}, drop_all_but<1>),
    Rule(left_brace, {{left_brace}, {newline}}, drop_all_but<0>),
    Rule(right_brace, {{newline}, {right_brace}}, drop_all_but<1>),

    Rule(expression, {{expression}, {expression}}, drop_all_but<0>,
         ParserMode::BadLine)
    /* End miscellaneous */

};

#undef ARGS
#undef STMT 
}  // namespace Language
