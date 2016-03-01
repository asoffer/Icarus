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

  constexpr size_t prec_value(size_t n, size_t assoc) { return (n << 2) + assoc; }

  // Associativity stored in the lowest two bits.

  size_t precedence(Operator op) {
    switch (op) {
#define OPERATOR_MACRO(name, symbol, prec, assoc) \
      case Operator::name: return prec_value((prec), (assoc));
#include "config/operator.conf"
#undef OPERATOR_MACRO
    }
  }

  const std::map<std::string, Operator> lookup_operator = {
#define OPERATOR_MACRO(name, symbol, prec, assoc) \
    { #symbol, Operator::name },
#include "config/operator.conf"
#undef OPERATOR_MACRO
  };

  // Here is the definition for all rules in the langugae. For a rule to be
  // applied, the node types on the top of the stack must match those given in
  // the list (second line of each rule). If so, then the function given in the
  // third line of each rule is applied, replacing the matched nodes. Lastly,
  // the new nodes type is set to the given type in the first line.
  const std::vector<Rule> rules = {
    /* Begin literals */
    Rule(expression, { {reserved_true} },   AST::Terminal::build_true),
    Rule(expression, { {reserved_false} },  AST::Terminal::build_false),
    Rule(expression, { {reserved_null} },   AST::Terminal::build_null),
    Rule(expression, { {identifier} },      AST::Identifier::build),
    Rule(expression, { {uint_literal} },    AST::Terminal::build_uint_literal),
    Rule(expression, { {int_literal} },     AST::Terminal::build_int_literal),
    Rule(expression, { {real_literal} },    AST::Terminal::build_real_literal),
    Rule(expression, { {string_literal} },  AST::Terminal::build_string_literal),
    Rule(expression, { {char_literal} },    AST::Terminal::build_char_literal),
    Rule(expression, { {reserved_ascii} },  AST::Terminal::build_ASCII),
    Rule(expression, { {reserved_alloc} },  AST::Terminal::build_alloc),
    Rule(expression, { {fn_literal, fn_expression} }, drop_all_but<0>),
    Rule(expression, { {type_literal, reserved_type} },
        AST::Terminal::build_type_literal),

    Rule(fn_literal,
        { {fn_expression}, {left_brace}, {statements}, {right_brace} },
        AST::FunctionLiteral::build),

    // TODO rename this type. could be an array type or an array expression depending on the context
    Rule(expression,
        { {left_bracket}, {expression}, {semicolon}, {expression}, {right_bracket} },
        AST::ArrayType::build),

    // TODO make this the correct thing
    Rule(expression,
        { {left_bracket}, {negation}, {semicolon}, {expression}, {right_bracket} },
        AST::ArrayType::build_unknown),
    /* End literals */

    /* Begin declaration */
    Rule(declaration, { {identifier}, {decl_assign_operator}, {expression} },
        AST::Declaration::build_assign),

    Rule(declaration,
        { {identifier}, {decl_operator}, {expression, fn_expression} },
        AST::Declaration::build_decl),

    // TODO Should this be an expression or declaration
    Rule(declaration,
        { {left_paren}, {fn_declaration, declaration, declaration_comma_list}, {right_paren} },
        AST::Expression::parenthesize),

    Rule(declaration_comma_list,
        { {declaration_comma_list, declaration}, {comma}, {declaration} },
        AST::ChainOp::build),
    /* End declaration */

    /* Begin assignment */
    Rule(assignment,
        { {fn_declaration, declaration, expression}, {assign_operator}, {expression, fn_expression, fn_literal} },
        AST::Assignment::build),

    Rule(assignment, { {left_paren}, {assignment}, {right_paren} },
        AST::Expression::parenthesize),

    Rule(fn_assignment, { {left_paren}, {fn_assignment}, {right_paren} },
        AST::Expression::parenthesize),
    /* End assignment */
    
    /* Begin expression */
    Rule(expression, { {left_paren}, {expression}, {right_paren} },
        AST::Expression::parenthesize),

    Rule(expression,
        { {dereference, negation, indirection, reserved_print, reserved_return, reserved_free}, {expression} },
        AST::Unop::build),

    Rule(fn_expression,
        { {left_paren}, {fn_expression}, {right_paren} },
        AST::Expression::parenthesize),

    Rule(expression,
        { {expression}, {negation, generic_operator}, {expression} },
        AST::Binop::build),

    Rule(expression,
        { {expression}, {dot}, {identifier} },
        AST::Access::build),

    Rule(expression,
        { {expression}, {indirection, bool_operator, binary_boolean_operator}, {expression} },
        AST::ChainOp::build),

    Rule(fn_expression,
        { {expression, declaration}, {fn_arrow}, {expression} },
        AST::Binop::build),
    /* End expression */

    /* Begin void return */
    Rule(void_return_expression, { {reserved_return}, {newline}},
        AST::Terminal::build_void_return),
    /* End void return */

    /* Begin paren/bracket operators */
    Rule(expression,
        { {expression}, {left_paren}, {expression}, {right_paren} },
        AST::Binop::build_paren_operator),

    Rule(expression,
        { {expression}, {left_paren}, {right_paren} },
        AST::Unop::build_paren_operator),

    Rule(expression,
        { {expression}, {left_bracket}, {expression}, {right_bracket} },
        AST::Binop::build_bracket_operator),

    Rule(expression,
        { {left_bracket}, {expression}, {right_bracket} },
        AST::ArrayLiteral::build),
    /* End paren/bracket operators */

    /* Begin if */
    Rule(if_statement,
        { {reserved_if}, {expression}, {left_brace}, {statements}, {right_brace} },
        AST::Conditional::build_if),

    Rule(if_statement,
        { {reserved_if}, {assignment}, {left_brace}, {statements}, {right_brace} },
        AST::Conditional::build_if_assignment_error),

    Rule(if_statement,
        { {if_statement}, {reserved_else}, {if_statement} },
        AST::Conditional::build_else_if),

    Rule(if_else_statement,
        { {if_statement}, {reserved_else}, {left_brace}, {statements}, {right_brace} },
        AST::Conditional::build_else),

    Rule(if_else_statement,
        { {if_else_statement}, {reserved_else}, {left_brace}, {statements}, {right_brace} },
        AST::Conditional::build_extra_else_error),

    Rule(if_else_statement,
        { {if_else_statement}, {reserved_else}, {if_statement} },
        AST::Conditional::build_extra_else_if_error),
    /* End if */

    /* Begin statements */
    Rule(statements,
        { {assignment, fn_assignment, declaration, fn_declaration, expression, if_statement, if_else_statement, while_statement, break_statement}, {newline} },
        AST::Statements::build_one),

    Rule(statements,
        { {statements}, {assignment, fn_assignment, declaration, fn_declaration, expression, if_statement, if_else_statement, while_statement, break_statement}, {newline} },
        AST::Statements::build_more),


    Rule(statements, { {void_return_expression} },
        AST::Statements::build_one),

    Rule(statements, { {statements}, {void_return_expression} },
        AST::Statements::build_more),

    Rule(statements, { {newline}, {statements} }, drop_all_but<1>),
    Rule(statements, { {statements}, {newline} }, drop_all_but<0>),
    /* End statements */

    /* Begin comma list */
    Rule(expression, { {expression}, {comma}, {expression} }, AST::ChainOp::build),
    /* End comma list */

    /* Begin case statements */
    Rule(key_value_pair_list,
        { {expression}, {rocket_operator}, {expression}, {newline} },
        AST::KVPairList::build_one),

    Rule(key_value_pair_list,
        { {key_value_pair_list}, {expression}, {rocket_operator}, {expression}, {newline} },
        AST::KVPairList::build_more),

    Rule(key_value_pair_list,
        { {reserved_else}, {rocket_operator}, {expression}, {newline} },
        AST::KVPairList::build_one),

    Rule(key_value_pair_list,
        { {key_value_pair_list}, {reserved_else}, {rocket_operator}, {expression}, {newline} },
        AST::KVPairList::build_more),

    Rule(key_value_pair_list, // An error, they probably meant `==` instead of `=`
        { {assignment}, {rocket_operator}, {expression}, {newline} },
        AST::KVPairList::build_one_assignment_error),

    Rule(key_value_pair_list, // An error, they probably meant `==` instead of `=`
        { {key_value_pair_list}, {assignment}, {rocket_operator}, {expression}, {newline} },
        AST::KVPairList::build_more_assignment_error),

    Rule(expression,
        { {reserved_case}, {left_brace}, {key_value_pair_list}, {right_brace} },
        AST::Case::build),
    /* End case statements */

    /* Begin while loop */
    Rule(while_statement,
        { {reserved_while}, {expression}, {left_brace}, {statements}, {right_brace} },
        AST::While::build),

    Rule(while_statement,
        { {reserved_while}, {assignment}, {left_brace}, {statements}, {right_brace} },
        AST::While::build_assignment_error),

    /* End while loop */

    /* Begin loop extras */
    Rule(break_statement, { {reserved_break} }, AST::Break::build),
    /* End loop extras */

    /* Begin type literals */
    // TODO tighten this up. Just taking in any statements probably captures
    // way too much.
    Rule(expression,
        { {reserved_struct}, {left_brace}, {statements}, {right_brace} },
        AST::TypeLiteral::build),
    /* End type literals */

    /* Begin enums */
    // TODO tighten this up. Just taking in any statements probably captures
    // way too much.
    Rule(expression,
        { {reserved_enum}, {left_brace}, {statements}, {right_brace} },
        AST::EnumLiteral::build),
    /* End enums */

    
    /* Begin import */
    Rule(newline, { {reserved_import}, {string_literal}, {newline} }, import_file),
    /* End import */


    /* Begin miscellaneous */
    Rule(newline,     { {newline}, {newline} },     drop_all_but<0>),
    Rule(left_brace,  { {newline}, {left_brace} },  drop_all_but<1>),
    Rule(left_brace,  { {left_brace}, {newline} },  drop_all_but<0>),
    Rule(right_brace, { {newline}, {right_brace} }, drop_all_but<1>),

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
