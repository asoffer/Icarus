#include "Parser.h"
#include "Rule.h"

namespace Language {
extern size_t precedence(Language::Operator op);

inline bool is_expression(NodeType t) { return (t & MASK_expression) != 0; }

inline bool is_binary_operator(NodeType t) {
  return (t & MASK_binary_operator) != 0;
}

inline bool is_decl(NodeType t) {
  return t == DECL_OPERATOR_INFER || t == DECL_OPERATOR_STD ||
         t == DECL_OPERATOR_GENERATE || t == reserved_in;
}

// This is intentionally not accessible in other translation units.
template <size_t N> AST::Node *drop_all_but(NPtrVec &&nodes) {
  auto temp = nodes[N];
  assert(temp && "stolen pointer is null");
  nodes[N] = nullptr;
  return temp;
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
  *tk_node     = AST::TokenNode::newline();
  return tk_node;
}

#define ARGS DECL_LIST, STMT_DECL_STD, STMT_DECL_INFER
// This doesn't allow include 'void' or other things like that.

#define STMT                                                                   \
  STMT_DECL_STD, STMT_DECL_INFER, STMT_IF, STMT_IF_ELSE, STMT_FOR, STMT_WHILE, \
      STMT_JUMP, STMT_ASSIGN

// Here are the definitions for all rules in the langugae. For a rule to be
// applied, the node types on the top of the stack must match those given in the
// list (second line of each rule). If so, then the function given in the third
// line of each rule is applied, replacing the matched nodes. Lastly, the new
// nodes type is set to the given type in the first line.
static const std::vector<Rule> rules = {
    /* Begin literals */
    Rule(expression, {Opt({identifier})}, AST::Identifier::build),

    Rule(expression, {Opt({string_literal})},
         AST::Terminal::build_string_literal),

    Rule(expression, {Opt({fn_literal, fn_expression})}, drop_all_but<0>),

    Rule(fn_literal, {Opt({fn_expression}), Opt({left_brace}),
                      Opt({statements}), Opt({right_brace})},
         AST::FunctionLiteral::build),

    // TODO rename this type. could be an array type or an array expression
    // depending on the context
    Rule(expression, {Opt({left_bracket}), Opt({expression}), Opt({semicolon}),
                      Opt({expression}), Opt({right_bracket})},
         AST::ArrayType::build),

    // TODO make this the correct thing
    Rule(expression, {Opt({left_bracket}), Opt({negation}), Opt({semicolon}),
                      Opt({expression}), Opt({right_bracket})},
         AST::ArrayType::build_unknown),
    /* End literals */

    /* Begin declaration */
    Rule(STMT_DECL_STD, {Opt({STMT_DECL_STD}), Opt({hashtag})},
         AST::Declaration::AddHashtag),

    Rule(STMT_DECL_STD, {Opt({identifier}), Opt({DECL_OPERATOR_STD}),
                         Opt({STMT_DECL_GENERATE, expression, fn_expression})},
         AST::Declaration::BuildStd),

    Rule(STMT_DECL_INFER, {Opt({identifier}), Opt({DECL_OPERATOR_INFER}),
                           Opt({expression, fn_expression})},
         AST::Declaration::BuildInfer),
    Rule(DECL_IN, {Opt({identifier}), Opt({reserved_in}), Opt({expression})},
         AST::Declaration::BuildIn),
    Rule(STMT_DECL_GENERATE,
         {Opt({identifier, expression}), Opt({DECL_OPERATOR_GENERATE}),
          Opt({identifier})}, // TODO Should idenifier be a first option?
         AST::Declaration::BuildGenerate),
 /* end declaration */
 
 /* Begin parentheses */
#define PAREN_RULE(node_type)                                                  \
  Rule(node_type, {Opt({left_paren}), Opt({node_type}), Opt({right_paren})},  \
        AST::Expression::parenthesize)

    PAREN_RULE(expression), PAREN_RULE(STMT_ASSIGN), PAREN_RULE(fn_expression),
    PAREN_RULE(STMT_DECL_STD), PAREN_RULE(STMT_DECL_INFER),
    PAREN_RULE(DECL_LIST),

#undef PAREN_RULE
    /* End parentheses */

    /* Begin declaration list */
    // TODO would this include ((a: int, b: int), c: int) and is that what
    // we
    // want?
    Rule(DECL_LIST,
         {Opt({ARGS}), Opt({comma}), Opt({STMT_DECL_STD, STMT_DECL_INFER})},
         AST::ChainOp::build),
    Rule(DECL_IN_LIST,
         {Opt({DECL_IN, DECL_IN_LIST}), Opt({comma}), Opt({DECL_IN})},
         AST::ChainOp::build),
    /* End declaration list */

    /* Begin assignment */
    Rule(STMT_ASSIGN, {Opt({STMT_DECL_STD, expression}), Opt({assign_operator}),
                       Opt({expression, fn_expression, fn_literal})},
         AST::Binop::build_assignment),
    /* End assignment */

    /* Begin expression */
    Rule(expression, {Opt({not_operator, dereference, negation, indirection,
                           reserved_print, reserved_return, reserved_free}),
                      Opt({expression})},
         AST::Unop::build),

    Rule(expression, {Opt({expression}), Opt({dots})}, AST::Unop::build_dots),

    Rule(expression,
         {Opt({expression}), Opt({generic_operator, dots, negation}),
          Opt({expression})},
         AST::Binop::build),

    Rule(expression, {Opt({expression}), Opt({dot}), Opt({identifier})},
         AST::Access::build),

    Rule(expression, {Opt({expression}), Opt({indirection, bool_operator,
                                              binary_boolean_operator}),
                      Opt({expression})},
         AST::ChainOp::build),

    Rule(fn_expression,
         {Opt({expression, ARGS}), Opt({fn_arrow}), Opt({expression})},
         AST::Binop::build),
    /* End expression */

    /* Begin paren/bracket operators */
    Rule(expression, {Opt({expression}), Opt({left_paren}), Opt({expression}),
                      Opt({right_paren})},
         AST::Binop::build_paren_operator),

    Rule(expression, {Opt({expression}), Opt({left_paren}), Opt({right_paren})},
         AST::Unop::build_paren_operator),

    Rule(expression, {Opt({expression}), Opt({left_bracket}), Opt({expression}),
                      Opt({right_bracket})},
         AST::Binop::build_bracket_operator),

    Rule(expression,
         {Opt({left_bracket}), Opt({expression}), Opt({right_bracket})},
         AST::ArrayLiteral::build),
    /* End paren/bracket operators */

    /* Begin if */
    Rule(STMT_IF, {Opt({reserved_if}), Opt({expression}), Opt({left_brace}),
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
    /* End if */

    /* Begin statements */
    Rule(statements, {Opt({STMT, expression}), Opt({newline})},
         AST::Statements::build_one),
    Rule(statements,
         {Opt({statements}), Opt({STMT, expression}), Opt({newline})},
         AST::Statements::build_more),

    Rule(statements, {Opt({statements}), Opt({newline})}, drop_all_but<0>),
    Rule(statements, {Opt({newline}), Opt({statements})}, drop_all_but<1>),
    /* End statements */

    /* Begin comma list */
    // TODO is this even used?
    Rule(expression, {Opt({expression}), Opt({comma}), Opt({expression})},
         AST::ChainOp::build),
    /* End comma list */

    /* Begin case statements */
    Rule(key_value_pair_list, {Opt({expression}), Opt({rocket_operator}),
                               Opt({expression}), Opt({newline})},
         AST::KVPairList::build_one),

    Rule(key_value_pair_list,
         {Opt({key_value_pair_list}), Opt({expression}), Opt({rocket_operator}),
          Opt({expression}), Opt({newline})},
         AST::KVPairList::build_more),

    Rule(key_value_pair_list, {Opt({reserved_else}), Opt({rocket_operator}),
                               Opt({expression}), Opt({newline})},
         AST::KVPairList::build_one),

    Rule(key_value_pair_list,
         {Opt({key_value_pair_list}), Opt({reserved_else}),
          Opt({rocket_operator}), Opt({expression}), Opt({newline})},
         AST::KVPairList::build_more),

    // An error, they probably meant `==` instead of `=`
    Rule(key_value_pair_list, {Opt({STMT_ASSIGN}), Opt({rocket_operator}),
                               Opt({expression}), Opt({newline})},
         AST::KVPairList::build_one_assignment_error),

    // An error, they probably meant `==` instead of `=`
    Rule(key_value_pair_list,
         {Opt({key_value_pair_list}), Opt({STMT_ASSIGN}),
          Opt({rocket_operator}), Opt({expression}), Opt({newline})},
         AST::KVPairList::build_more_assignment_error),

    // Case must be followed by a left brace
    Rule(reserved_case, {Opt({reserved_case}), Opt({left_brace}, false)},
         missing_lbrace_case),

    Rule(expression, {Opt({reserved_case}), Opt({left_brace}),
                      Opt({key_value_pair_list}), Opt({right_brace})},
         AST::Case::build),
    /* End case statements */

    /* Begin while loop */
    Rule(STMT_WHILE, {Opt({reserved_while}), Opt({expression}),
                      Opt({left_brace}), Opt({statements}), Opt({right_brace})},
         AST::While::build),
    Rule(STMT_WHILE, {Opt({reserved_while}), Opt({STMT_ASSIGN}),
                      Opt({left_brace}), Opt({statements}), Opt({right_brace})},
         AST::While::build_assignment_error),
    /* End while loop */

    /* Begin for loop */
    Rule(STMT_FOR, {Opt({reserved_for}), Opt({DECL_IN, DECL_IN_LIST}),
                    Opt({left_brace}), Opt({statements}), Opt({right_brace})},
         AST::For::build),
    /* End for loop */

    /* Begin loop extras */
    Rule(STMT_JUMP, {Opt({reserved_restart, reserved_break, reserved_repeat,
                          reserved_continue, reserved_return})},
         AST::Jump::build),
    /* End loop extras */

    /* Begin structs */
    Rule(expression, {Opt({reserved_struct}), Opt({left_brace}),
                      Opt({statements}), Opt({right_brace})},
         AST::StructLiteral::build),

    Rule(expression, {Opt({reserved_struct}), Opt({ARGS}), Opt({left_brace}),
                      Opt({statements}), Opt({right_brace})},
         AST::StructLiteral::build_parametric),

    Rule(reserved_enum,
         {Opt({reserved_enum}), Opt({left_brace, left_paren}, false)},
         missing_struct_encapsulator),

    /* End structs */

    /* Begin enums */
    Rule(expression, {Opt({reserved_enum}), Opt({left_brace}),
                      Opt({statements}), Opt({right_brace})},
         AST::EnumLiteral::build),

    // Enum must be followed by a left brace
    Rule(reserved_enum, {Opt({reserved_enum}), Opt({left_brace}, false)},
         missing_lbrace_enum),
    /* End enums */

    /* Begin import */
    Rule(newline,
         {Opt({reserved_import}), Opt({string_literal}), Opt({newline})},
         import_file),
    /* End import */

    /* Begin miscellaneous */
    Rule(comma, {Opt({comma}), Opt({newline})}, drop_all_but<0>),
    Rule(newline, {Opt({newline}), Opt({newline})}, drop_all_but<0>),
    Rule(left_brace, {Opt({newline}), Opt({left_brace})}, drop_all_but<1>),
    Rule(left_brace, {Opt({left_brace}), Opt({newline})}, drop_all_but<0>),
    Rule(right_brace, {Opt({newline}), Opt({right_brace})}, drop_all_but<1>),

    Rule(expression, {Opt({expression}), Opt({expression})}, drop_all_but<0>,
         ParserMode::BadLine)
    /* End miscellaneous */

};

#undef ARGS
#undef STMT 


} // namespace Language

namespace debug {
extern bool parser;
} // namespace debug

// Parse the file with a shift-reduce algorithm
AST::Node *Parser::parse() {
  // The very first token node is a newline.
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
      reduce_singletons();
      // Shift if you are supposed to, or if you are unable to reduce.
      if (should_shift() || !reduce()) { shift(); }
    } break;
    case ParserMode::BadLine: {
      ignore();
      if (lookahead_->node_type() == Language::newline) {
        mode_ = ParserMode::Good;
      }
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
  auto next_node_ptr = new AST::TokenNode;
  lexer_ >> *next_node_ptr;

  delete lookahead_;
  lookahead_ = next_node_ptr;
}

void Parser::shift() {
  auto next_node_ptr = new AST::TokenNode;
  lexer_ >> *next_node_ptr;

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
  lookahead_  = new AST::TokenNode;
  *lookahead_ = AST::TokenNode::newline();
}



// This function determines if a shift should be done, even when a valid
// reduce is possible. Recall that the stack can never be empty, so calls to
// stack_.back() are always safe.
bool Parser::should_shift() {
  // We'll need these node types a lot, so lets make it easy to use
  const auto last_type = stack_.back()->node_type();
  const auto ahead_type = lookahead_->node_type();

  if (last_type == Language::reserved_return) {
    return ahead_type != Language::newline;
  }

  // If we see an identifier followed by a decl_operator, shift. Without this
  // check present, an identifier would get changed to an expression, and the
  // verification that it really is something defineable would be more
  // computationally intensive.
  if (last_type == Language::identifier && Language::is_decl(ahead_type)) {
    return ahead_type != Language::DECL_OPERATOR_GENERATE;
  }

  if (stack_.size() >= 2 && ahead_type == Language::newline &&
      last_type == Language::string_literal &&
      stack_[stack_.size() - 2]->node_type() == Language::reserved_import) {
    return true;
  }

  // Reduce terminals
  switch (last_type) {
  case Language::identifier:
  case Language::string_literal:
  case Language::right_paren:
  case Language::right_bracket: return false;
  default:;
  }

  if (last_type == Language::dots) {
    // TODO simplify this
    switch (ahead_type) {
    case Language::identifier:
    case Language::reserved_true:
    case Language::reserved_false:
    case Language::int_literal:
    case Language::uint_literal:
    case Language::real_literal:
    case Language::char_literal:
    case Language::string_literal:
    case Language::type_literal: return true;
    default:;
    }

    return (Language::is_expression(ahead_type) ||
            ((ahead_type & Language::MASK_left_unary_operator) != 0));
  }

  // Shift all newlines together so they can be repeatedly reduced. Similarly,
  // shift a newline followed by a left_brace so the newline can be forgotten.
  // This allows us to have both
  //
  // if cond {
  //   ...
  // }
  //
  // and also
  //
  // if cond
  // {
  //   ...
  // }
  //
  if (last_type == Language::newline &&
      (ahead_type == Language::left_brace || ahead_type == Language::newline)) {
    return true;
  }

  // For function calls, shift the parentheses (same for indexing)
  if (Language::is_expression(last_type) &&
      (ahead_type == Language::left_paren ||
       ahead_type == Language::left_bracket) &&
      // Must also guarantee that we don't have a '.' beforehand
      (stack_.size() < 2 ||
       stack_[stack_.size() - 2]->node_type() != Language::dot)) {
    return true;
  }

  // If we have two competing operators, one on the stack and a binary operator
  // in the lookahead, then we must compare their precedence levels to decide
  // whether or not to shift. We only shift if the one on the stack has lower
  // precedence.
  if (Language::is_binary_operator(ahead_type) && stack_.size() >= 2 &&
      Language::is_operator(stack_[stack_.size() - 2]->node_type())) {

    size_t lhs_prec;
    size_t rhs_prec = Language::precedence(lookahead_->op);

    const auto &prev_node = stack_[stack_.size() - 2];
    if (prev_node->is_token_node()) {
      auto prev_token_node = static_cast<AST::TokenNode *>(prev_node);
      lhs_prec = Language::precedence(prev_token_node->op);

    } else {
      assert(false && "Previous node is not a token node.");
    }

    if (lhs_prec != rhs_prec)
      return lhs_prec < rhs_prec;

    auto associativity = lhs_prec & assoc_mask;

    // Non-associative operators in this situation are a parsing error,
    // because the lhs and rhs precedences are the same.
    //
    // TODO figure out if we should exit early here. Is there any reasonable
    // way to continue?
    if (associativity == non_assoc) {
      error_log.log(lookahead_->loc, "Non-associative operator found "
                                     "with no specified association. "
                                     "Maybe you forgot parentheses?");
    }

    // If the precedence levels are equal, we should shift right-associative
    // operators. Chain operators are built as if they were left-associative.
    return associativity == right_assoc;
  }

  // If we're defining a function with braces don't stop early.
  if ((last_type == Language::fn_expression ||
       last_type == Language::reserved_struct) &&
      ahead_type == Language::left_brace) {
    return true;
  }

  return false;
}

bool Parser::reduce_singletons() {
#define SINGLETON(output, nt, func)                                            \
  if (stack_.back()->node_type() == Language::nt) {                            \
    auto nptr = stack_.back();                                                 \
    stack_.pop_back();                                                         \
    stack_.push_back(func({nptr}));                                            \
    stack_.back()->set_node_type(Language::output);                            \
    delete nptr;                                                               \
    return true;                                                               \
  }

  SINGLETON(expression, reserved_true, AST::Terminal::build_true)
  SINGLETON(expression, reserved_false, AST::Terminal::build_false)
  SINGLETON(expression, reserved_null, AST::Terminal::build_null)
  SINGLETON(expression, uint_literal, AST::Terminal::build_uint_literal)
  SINGLETON(expression, int_literal, AST::Terminal::build_int_literal)
  SINGLETON(expression, real_literal, AST::Terminal::build_real_literal)
  SINGLETON(expression, char_literal, AST::Terminal::build_char_literal)
  SINGLETON(expression, reserved_input, AST::Terminal::build_input)
  SINGLETON(expression, reserved_ord, AST::Terminal::build_ord)
  SINGLETON(expression, reserved_ascii, AST::Terminal::build_ASCII)
  SINGLETON(expression, reserved_alloc, AST::Terminal::build_alloc)
  SINGLETON(expression, type_literal, AST::Terminal::build_type_literal)
#undef SINGLETON
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
