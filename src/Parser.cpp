#include "Parser.h"
#include "Rule.h"

template <size_t N> AST::Node *drop_all_but(NPtrVec &&nodes) {
  auto temp = nodes[N];
  assert(temp && "stolen pointer is null");
  nodes[N] = nullptr;
  return temp;
}

namespace ErrMsg {
AST::Node *ExpectedRightParen(NPtrVec &&nodes) {
  error_log.log(nodes[1]->loc, "Expected ')'.");
  nodes.push_back(new AST::TokenNode(nodes[1]->loc, Language::right_paren));
  return AST::Expression::parenthesize(std::forward<NPtrVec &&>(nodes));
}

AST::Node *MissingLeftParen(NPtrVec &&nodes) {
  error_log.log(nodes[1]->loc, "Missing '('.");
  return drop_all_but<0>(std::forward<NPtrVec &&>(nodes));
}

namespace Decl {
AST::Node *BadRHS(NPtrVec &&nodes) {
  error_log.log(nodes[1]->loc, "Invalid right-hand side of declaration.");
  return drop_all_but<0>(std::forward<NPtrVec &&>(nodes));
}

AST::Node *BadLHS(NPtrVec &&nodes) {
  error_log.log(nodes[1]->loc,
                "left-hand side of declaration must be an identifier.");
  return drop_all_but<0>(std::forward<NPtrVec &&>(nodes));
}

AST::Node *BadBoth(NPtrVec &&nodes) {
  error_log.log(nodes[1]->loc,
                "left-hand side of declaration must be an identifier.");
  error_log.log(nodes[1]->loc, "Invalid right-hand side of declaration.");
  return drop_all_but<0>(std::forward<NPtrVec &&>(nodes));
}
} // namespace Decl

namespace Unop {
AST::Node *AtEOF(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc, "Unary operator at end of file.");
  return drop_all_but<0>(std::forward<NPtrVec &&>(nodes));
}

AST::Node *AtNewline(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc, "Unexpected unary operator at end of line.");
  return drop_all_but<1>(std::forward<NPtrVec &&>(nodes));
}

AST::Node *Stray(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc, "Unexpected unary operator preceding '" +
                                   nodes[1]->token() + "'");
  return drop_all_but<1>(std::forward<NPtrVec &&>(nodes));
}
} // namespace Unop

namespace Binop {
namespace LHS {
AST::Node *AtNewline(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc, "Unexpected binary operator at start of line.");
  return drop_all_but<0>(std::forward<NPtrVec &&>(nodes));
}

AST::Node *Stray(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc, "Unexpected binary operator following '" +
                                   nodes[0]->token() + "'");
  return drop_all_but<0>(std::forward<NPtrVec &&>(nodes));
}
} // namespace LHS

namespace RHS {
AST::Node *AtEOF(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc, "Binary operator at end of file.");
  return drop_all_but<2>(std::forward<NPtrVec &&>(nodes));
}

AST::Node *AtNewline(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc, "Unexpected binary operator at end of line.");
  return drop_all_but<1>(std::forward<NPtrVec &&>(nodes));
}

AST::Node *Stray(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc, "Unexpected binary operator preceding '" +
                                   nodes[2]->token() + "'");
  return drop_all_but<2>(std::forward<NPtrVec &&>(nodes));
}
} // namespace RHS

AST::Node *Nonsense(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc, "Unexpected binary operator.");
  return drop_all_but<0>(std::forward<NPtrVec &&>(nodes));
}
} // namespace Binop
} // namespace ErrMsg

namespace Language {
extern size_t precedence(Language::Operator op);

inline bool is_expression(NodeType t) { return (t & MASK_expression) != 0; }

inline bool is_binop(NodeType t) { return (t & MASK_binary_operator) != 0; }
inline bool is_unop(NodeType t) { return (t & MASK_left_unary_operator) != 0; }

inline bool is_decl(NodeType t) {
  return t == colon || t == colon_eq || t == DECL_OPERATOR_GENERATE ||
         t == reserved_in;
}

#define EXPECTED_ENCAPSULATOR(leading_keyword, quoted_symbol, name)            \
  AST::Node *missing_##name##_##leading_keyword(NPtrVec &&nodes) {             \
    error_log.log(nodes[0]->loc, "Expected '" quoted_symbol                    \
                                 "' following keyword '" #leading_keyword      \
                                 "', but '" +                                  \
                                     nodes[1]->token() + "' found instead.");  \
    return drop_all_but<1>(std::forward<NPtrVec &&>(nodes));                   \
  }

EXPECTED_ENCAPSULATOR(case, "{", lbrace)
EXPECTED_ENCAPSULATOR(enum, "{", lbrace)

AST::Node *missing_struct_encapsulator(NPtrVec &&nodes) {
  error_log.log(nodes[0]->loc,
                "Expected '(' or '{' following keyword 'struct', but" +
                    nodes[1]->token() + "' found instead.");
  return drop_all_but<1>(std::forward<NPtrVec &&>(nodes));
}

// TODO we can't have a '/' character, and since all our programs are in the
// programs/ directory for now, we hard-code that. This needs to be removed.
AST::Node *import_file(NPtrVec &&nodes) {
  file_queue.emplace("programs/" + nodes[1]->token());
  auto tk_node = new AST::TokenNode;
  *tk_node     = AST::TokenNode::newline(nodes[0]->loc);
  return tk_node;
}

#define STRICT_UNOP not_operator, dereference, reserved_print, reserved_free
#define UNOP_AND_BINOP negation, indirection
#define UNOP STRICT_UNOP, UNOP_AND_BINOP
#define BINOP generic_operator, fn_arrow, UNOP_AND_BINOP
#define NON_FN_EXPR expression, identifier, declaration
#define EXPR fn_expression, NON_FN_EXPR, fn_literal

#define ARGS decl_list, declaration
#define STMT                                                                   \
  declaration, STMT_IF, STMT_IF_ELSE, STMT_FOR, STMT_WHILE, STMT_JUMP,         \
      STMT_ASSIGN

// Here are the definitions for all rules in the langugae. For a rule to be
// applied, the node types on the top of the stack must match those given in the
// list (second line of each rule). If so, then the function given in the third
// line of each rule is applied, replacing the matched nodes. Lastly, the new
// nodes type is set to the given type in the first line.
static const std::vector<Rule> rules = {
    /* Rules for unary operator */
    Rule(expression, {Opt({UNOP}), Opt({NON_FN_EXPR})}, AST::Unop::build),
//    Rule(keep_current, {Opt({UNOP}), Opt({newline})}, ErrMsg::Unop::AtNewline,
//         ParserMode::BadLine),
//    Rule(keep_current, {Opt({UNOP}), Opt({eof})}, ErrMsg::Unop::AtEOF,
//         ParserMode::BadLine),
//    Rule(keep_current, {Opt({UNOP}), Opt({NON_FN_EXPR, eof, newline}, false)},
//         ErrMsg::Unop::Stray, ParserMode::BadLine),

    /* Rules for binary operator */
    Rule(expression,
         {Opt({NON_FN_EXPR}), Opt({generic_operator}), Opt({NON_FN_EXPR})},
         AST::Binop::build),

//    // Error on RHS
//    Rule(keep_current, {Opt({NON_FN_EXPR}), Opt({BINOP}), Opt({newline})},
//         ErrMsg::Binop::RHS::AtNewline, ParserMode::BadLine),
//    Rule(keep_current, {Opt({NON_FN_EXPR}), Opt({BINOP}), Opt({eof})},
//         ErrMsg::Binop::RHS::AtEOF, ParserMode::BadLine),
//    Rule(keep_current, {Opt({NON_FN_EXPR}), Opt({BINOP}),
//                        Opt({NON_FN_EXPR, eof, newline}, false)},
//         ErrMsg::Binop::RHS::Stray, ParserMode::BadLine),
//    // Error on LHS
//    Rule(keep_current, {Opt({eof}), Opt({BINOP}), Opt({NON_FN_EXPR})},
//         ErrMsg::Binop::LHS::AtNewline, ParserMode::BadLine),
//    Rule(keep_current,
//         {Opt({NON_FN_EXPR, newline}, false), Opt({BINOP}), Opt({NON_FN_EXPR})},
//         ErrMsg::Binop::LHS::Stray, ParserMode::BadLine),
//    // Error on both sides
//    Rule(keep_current,
//         {Opt({NON_FN_EXPR}, false), Opt({BINOP}), Opt({NON_FN_EXPR}, false)},
//         ErrMsg::Binop::Nonsense, ParserMode::BadLine),

    /* Rules for :, := declarations */
    Rule(declaration, {Opt({identifier}), Opt({colon}), Opt({EXPR})},
         AST::Declaration::BuildStd),
//    Rule(declaration, {Opt({identifier}), Opt({colon}), Opt({EXPR}, false)},
//         ErrMsg::Decl::BadRHS, ParserMode::BadLine),
//    Rule(declaration, {Opt({identifier}, false), Opt({colon}), Opt({EXPR})},
//         ErrMsg::Decl::BadLHS, ParserMode::BadLine),
//    Rule(declaration,
//         {Opt({identifier}, false), Opt({colon}), Opt({EXPR}, false)},
//         ErrMsg::Decl::BadBoth, ParserMode::BadLine),
//
    Rule(declaration, {Opt({identifier}), Opt({colon_eq}), Opt({EXPR})},
         AST::Declaration::BuildInfer),
//    Rule(declaration, {Opt({identifier}), Opt({colon_eq}), Opt({EXPR}, false)},
//         ErrMsg::Decl::BadRHS, ParserMode::BadLine),
//    Rule(declaration, {Opt({identifier}, false), Opt({colon_eq}), Opt({EXPR})},
//         ErrMsg::Decl::BadLHS, ParserMode::BadLine),
//    Rule(declaration,
//         {Opt({identifier}, false), Opt({colon_eq}), Opt({EXPR}, false)},
//         ErrMsg::Decl::BadBoth, ParserMode::BadLine),

    ///////////////// SAFETY VERIFIED FOR RULES ABOVE HERE /////////////////
    Rule(decl_list,
         {Opt({declaration, decl_list}), Opt({comma}), Opt({declaration})},
         AST::ChainOp::build),
    Rule(expression, {Opt({expression}), Opt({comma}), Opt({expression})},
         AST::ChainOp::build),

    Rule(expression, {Opt({NON_FN_EXPR}), Opt({negation}), Opt({NON_FN_EXPR})},
         AST::Binop::build),

    /* Rules for parentheses */
    Rule(keep_current,
         {Opt({left_paren}), Opt({EXPR, decl_list}), Opt({right_paren})},
         AST::Expression::parenthesize),
    Rule(keep_current,
         {Opt({left_paren}), Opt({EXPR, decl_list}), Opt({right_paren}, false)},
         ErrMsg::ExpectedRightParen),
    Rule(keep_current, {Opt({EXPR, decl_list}), Opt({right_paren})},
         ErrMsg::MissingLeftParen, ParserMode::BadLine),

    // Old rules. unverified!

    Rule(expression, {Opt({reserved_return}), Opt({EXPR})}, AST::Unop::build),

    Rule(fn_literal, {Opt({fn_expression}), Opt({left_brace}),
                      Opt({statements}), Opt({right_brace})},
         AST::FunctionLiteral::build),

    Rule(expression, {Opt({left_bracket}), Opt({EXPR}), Opt({semicolon}),
                      Opt({expression}), Opt({right_bracket})},
         AST::ArrayType::build),

    Rule(declaration, {Opt({declaration}), Opt({hashtag})},
         AST::Declaration::AddHashtag),

    // More related above the fold
    // TODO redo declarations (especially for generics)

    // NOTE: this could be joined with something above the fold (verification
    // line) but this part hsan't been verified, so we don't
    Rule(keep_current,
         {Opt({left_paren}), Opt({STMT_ASSIGN}), Opt({right_paren})},
         AST::Expression::parenthesize),

    Rule(expression, {Opt({EXPR}), Opt({dots})}, AST::Unop::build_dots),

    Rule(expression, {Opt({EXPR}), Opt({dots}), Opt({EXPR})},
         AST::Binop::build),

    Rule(expression, {Opt({EXPR}), Opt({dot}), Opt({identifier})},
         AST::Access::build),

    Rule(expression, {Opt({EXPR}), Opt({indirection, bool_operator,
                                        binary_boolean_operator}),
                      Opt({EXPR})},
         AST::ChainOp::build),

    Rule(expression,
         {Opt({EXPR}), Opt({left_paren}), Opt({EXPR}), Opt({right_paren})},
         AST::Binop::build_paren_operator),

    Rule(expression, {Opt({EXPR}), Opt({left_paren}), Opt({right_paren})},
         AST::Unop::build_paren_operator),

    Rule(expression,
         {Opt({EXPR}), Opt({left_bracket}), Opt({EXPR}), Opt({right_bracket})},
         AST::Binop::build_bracket_operator),

    Rule(expression, {Opt({left_bracket}), Opt({EXPR}), Opt({right_bracket})},
         AST::ArrayLiteral::build),

    Rule(STMT_IF, {Opt({reserved_if}), Opt({EXPR}), Opt({left_brace}),
                   Opt({statements}), Opt({right_brace})},
         AST::Conditional::build_if),
    Rule(STMT_IF, {Opt({reserved_if}), Opt({STMT_ASSIGN}), Opt({left_brace}),
                   Opt({statements}), Opt({right_brace})},
         AST::Conditional::build_if_assignment_error),
    Rule(STMT_IF, {Opt({STMT_IF}), Opt({reserved_else}), Opt({STMT_IF})},
         AST::Conditional::build_else_if),
    Rule(STMT_IF_ELSE, {Opt({STMT_IF}), Opt({reserved_else}), Opt({left_brace}),
                        Opt({statements}), Opt({right_brace})},
         AST::Conditional::build_else),
    Rule(STMT_IF_ELSE,
         {Opt({STMT_IF_ELSE}), Opt({reserved_else}), Opt({left_brace}),
          Opt({statements}), Opt({right_brace})},
         AST::Conditional::build_extra_else_error),
    Rule(STMT_IF_ELSE,
         {Opt({STMT_IF_ELSE}), Opt({reserved_else}), Opt({STMT_IF})},
         AST::Conditional::build_extra_else_if_error),

    Rule(STMT_WHILE, {Opt({reserved_while}), Opt({EXPR}), Opt({left_brace}),
                      Opt({statements}), Opt({right_brace})},
         AST::While::build),
    Rule(STMT_WHILE, {Opt({reserved_while}), Opt({STMT_ASSIGN}),
                      Opt({left_brace}), Opt({statements}), Opt({right_brace})},
         AST::While::build_assignment_error),

    Rule(key_value_pair_list,
         {Opt({EXPR}), Opt({rocket_operator}), Opt({EXPR}), Opt({newline})},
         AST::KVPairList::build_one),

    Rule(key_value_pair_list,
         {Opt({key_value_pair_list}), Opt({EXPR}), Opt({rocket_operator}),
          Opt({EXPR}), Opt({newline})},
         AST::KVPairList::build_more),

    Rule(key_value_pair_list, {Opt({reserved_else}), Opt({rocket_operator}),
                               Opt({EXPR}), Opt({newline})},
         AST::KVPairList::build_one),

    Rule(key_value_pair_list,
         {Opt({key_value_pair_list}), Opt({reserved_else}),
          Opt({rocket_operator}), Opt({EXPR}), Opt({newline})},
         AST::KVPairList::build_more),

    // An error, they probably meant `==` instead of `=`
    Rule(key_value_pair_list, {Opt({STMT_ASSIGN}), Opt({rocket_operator}),
                               Opt({EXPR}), Opt({newline})},
         AST::KVPairList::build_one_assignment_error),

    // An error, they probably meant `==` instead of `=`
    Rule(key_value_pair_list,
         {Opt({key_value_pair_list}), Opt({STMT_ASSIGN}),
          Opt({rocket_operator}), Opt({EXPR}), Opt({newline})},
         AST::KVPairList::build_more_assignment_error),

    // Case must be followed by a left brace
    Rule(reserved_case, {Opt({reserved_case}), Opt({left_brace}, false)},
         missing_lbrace_case),

    Rule(expression, {Opt({reserved_case}), Opt({left_brace}),
                      Opt({key_value_pair_list}), Opt({right_brace})},
         AST::Case::build),

    Rule(STMT_FOR, {Opt({reserved_for}), Opt({DECL_IN, DECL_IN_LIST}),
                    Opt({left_brace}), Opt({statements}), Opt({right_brace})},
         AST::For::build),

    Rule(STMT_JUMP, {Opt({reserved_restart, reserved_break, reserved_repeat,
                          reserved_continue, reserved_return})},
         AST::Jump::build),

    Rule(expression, {Opt({reserved_struct}), Opt({left_brace}),
                      Opt({statements}), Opt({right_brace})},
         AST::StructLiteral::build),

    Rule(expression, {Opt({reserved_struct}), Opt({ARGS}), Opt({left_brace}),
                      Opt({statements}), Opt({right_brace})},
         AST::StructLiteral::build_parametric),

    Rule(DECL_IN, {Opt({identifier}), Opt({reserved_in}), Opt({EXPR})},
         AST::Declaration::BuildIn),
    Rule(DECL_IN_LIST,
         {Opt({DECL_IN, DECL_IN_LIST}), Opt({comma}), Opt({DECL_IN})},
         AST::ChainOp::build),


    Rule(reserved_enum,
         {Opt({reserved_enum}), Opt({left_brace, left_paren}, false)},
         missing_struct_encapsulator),

    Rule(expression, {Opt({reserved_enum}), Opt({left_brace}),
                      Opt({statements}), Opt({right_brace})},
         AST::EnumLiteral::build),

    Rule(fn_expression,
         {Opt({expression, ARGS}), Opt({fn_arrow}), Opt({expression})},
         AST::Binop::build),

    // Enum must be followed by a left brace
    Rule(reserved_enum, {Opt({reserved_enum}), Opt({left_brace}, false)},
         missing_lbrace_enum),

    Rule(newline, {Opt({reserved_import}), Opt({EXPR}), Opt({newline})},
         import_file),

    Rule(STMT_ASSIGN, {Opt({EXPR}), Opt({assign_operator}), Opt({EXPR})},
         AST::Binop::build_assignment),

    Rule(statements, {Opt({STMT, EXPR}), Opt({newline})},
         AST::Statements::build_one),

    Rule(statements, {Opt({statements}), Opt({STMT, EXPR}), Opt({newline})},
         AST::Statements::build_more),

    Rule(statements, {Opt({statements}), Opt({newline})}, drop_all_but<0>),
    Rule(statements, {Opt({newline}), Opt({statements})}, drop_all_but<1>),

    Rule(comma, {Opt({comma}), Opt({newline})}, drop_all_but<0>),
    Rule(newline, {Opt({newline}), Opt({newline})}, drop_all_but<0>),
    Rule(left_brace, {Opt({newline}), Opt({left_brace})}, drop_all_but<1>),
    Rule(left_brace, {Opt({left_brace}), Opt({newline})}, drop_all_but<0>),
    Rule(right_brace, {Opt({newline}), Opt({right_brace})}, drop_all_but<1>),
    Rule(expression, {Opt({expression}), Opt({expression})}, drop_all_but<0>,
         ParserMode::BadLine)

};

#undef STRICT_UNOP
#undef UNOP_AND_BINOP
#undef ANY_UNOP
#undef BINOP
#undef NON_FN_EXPR
#undef EXPR
#undef STMT

} // namespace Language

namespace debug {
extern bool parser;
} // namespace debug

// Parse the file with a shift-reduce algorithm
AST::Node *Parser::parse() {
  assert(lookahead_->node_type() == Language::newline);

  // Any valid program will clean this up eventually. Therefore, shifting on the
  // newline will not hurt us. The benefit of shifting is that we have now
  // enforced the invariant that the stack is never empty. This means we do not
  // need to check for an empty stack in the should_shift method.
  shift();

  while (true) { // Main parsing loop start
    switch (mode_) {
    case ParserMode::Same: assert(false && "This mode should be impossible");
    case ParserMode::Good: {
      // Shift if you are supposed to, or if you are unable to reduce.
      if (should_shift() || !reduce()) { shift(); }
    } break;
    case ParserMode::BadLine: {
      size_t line_num = stack_.back()->loc.line_num;

      // Kill the whole line backwards
      while (!stack_.empty() && stack_.back()->loc.line_num == line_num) {
        stack_.pop_back();
      }

      while (lookahead_->node_type() != Language::newline) { ignore(); }
      mode_ = ParserMode::Good;

    } break;
    case ParserMode::BadBlock:
    case ParserMode::BadFile:
    case ParserMode::Done: return cleanup();
    }

    if (debug::parser) { show_debug(); }

    if (lookahead_->node_type() == Language::eof) { mode_ = ParserMode::Done; }
  } // Main parsing loop end
}

AST::Node *Parser::cleanup() {
  while (reduce()) {
    if (debug::parser) { show_debug(); }
  }

  if (stack_.size() > 1) {
    if (debug::parser) {
      std::cerr << "Parser error: Exiting with Stack size = " << stack_.size()
                << std::endl;
    }
    error_log.log(TokenLocation(), "Parser error.");
  }

  return stack_.back();
}

// Print out the debug information for the parse stack, and pause.
void Parser::show_debug() const {
  // Clear the screen
  std::cout << "\033[2J\033[1;1H" << std::endl;
  for (const auto &node_ptr : stack_) {
    std::cout << *node_ptr;
  }

  std::cin.ignore(1);
}

void Parser::ignore() {
  auto next_node_ptr = lexer_.Next();

  delete lookahead_;
  lookahead_ = next_node_ptr;
}

void Parser::shift() {
  auto next_node_ptr = lexer_.Next();

  // Never shift comments onto the stack
  if (next_node_ptr->node_type() == Language::comment) {

    shift();
    return;
  }

  stack_.push_back(lookahead_);
  lookahead_ = next_node_ptr;
}

// Construct a parser for the given file
Parser::Parser(const std::string &filename)
    : lookahead_(nullptr), lexer_(filename), mode_(ParserMode::Good) {
  assert(stack_.empty());
  // Start the lookahead with a newline token. This is a simple way to ensure
  // proper initialization, because the newline will essentially be ignored.
  lookahead_ = new AST::TokenNode(TokenLocation(), Language::newline);
}

// This function determines if a shift should be done, even when a valid
// reduce is possible. Recall that the stack can never be empty, so calls to
// stack_.back() are always safe.
bool Parser::should_shift() {
  // We'll need these node types a lot, so lets make it easy to use
  const auto last_type = stack_.back()->node_type();
  const auto ahead_type = lookahead_->node_type();

  if (ahead_type == Language::left_paren ||
      ahead_type == Language::left_bracket) {
    return true;
  }

  if (last_type == Language::left_paren ||
      last_type == Language::left_bracket) {
    return true;
  }
  if (Language::is_operator(last_type)) { return true; }

  if (last_type == Language::newline &&
      (ahead_type == Language::left_brace || ahead_type == Language::newline)) {
    return true;
  }

  if ((last_type == Language::fn_expression ||
       last_type == Language::reserved_struct ||
       last_type == Language::reserved_case) &&
      (ahead_type == Language::left_brace || ahead_type == Language::newline)) {
    return true;
  }

  if (Language::is_binop(ahead_type) && stack_.size() >= 2 &&
      Language::is_operator(stack_[stack_.size() - 2]->node_type())) {
    size_t lhs_prec;

    assert(lookahead_->is_token_node());
    size_t rhs_prec = Language::precedence(((AST::TokenNode *)lookahead_)->op);

    const auto &prev_node = stack_[stack_.size() - 2];
    assert(prev_node->is_token_node() && "Previous node is not a TokenNode");
    auto prev_token_node = (AST::TokenNode *)prev_node;
    lhs_prec             = Language::precedence(prev_token_node->op);

    // If the precedence levels don't match, shift when the lhs has lower
    // precedence than the rhs. That is, in 'a + b * c', we should shift the '*'
    // rather than reduce 'a + b'. On the other hand, in 'a * b + c', we should
    // reduce 'a * b' rather than shift the '+'.
    if (lhs_prec != rhs_prec) { return lhs_prec < rhs_prec; }

    auto associativity = lhs_prec & assoc_mask;

    // Non-associative operators in this situation are a parsing error, because
    // the lhs and rhs precedences are the same.
    //
    // TODO figure out if we should exit early here. Is there any reasonable
    // way to continue?
    if (associativity == non_assoc) {
      error_log.log(lookahead_->loc, "Non-associative operator found "
                                     "with no specified association. "
                                     "Maybe you forgot parentheses?");
      assert(false);
    }
  }
  return false;
}

// Reduces the stack according to the language rules spceified in Language.cpp.
// Returns true if a rule is matched and applied. Returns false otherwise.
bool Parser::reduce() {
  const Rule *matched_rule_ptr = nullptr;
  for (const Rule &rule : Language::rules) {
    // If we've already matched a rule, ignore rules of lower precedence.
    // Precedence is simply determined by the length of the match.
    if (matched_rule_ptr != nullptr && matched_rule_ptr->size() > rule.size()) {
      continue;
    }

    if (rule.match(stack_)) {
      assert((matched_rule_ptr == nullptr ||
              rule.size() != matched_rule_ptr->size()) &&
             "Two rules matched with the same size");

      // Extract a pointer to the rule. It's safe to take a pointer here,
      // because
      // Language::rules is const.
      matched_rule_ptr = &rule;
    }
  }

  // If you make it to the end of the rules and still haven't matched, then
  // return false
  if (matched_rule_ptr == nullptr) { return false; }

  matched_rule_ptr->apply(stack_, mode_);

  return true;
}

