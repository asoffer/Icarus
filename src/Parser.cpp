#include "Parser.h"
#include "AST.h"

namespace debug {
  extern bool parser;
}  // namespace debug

// Construct a parser for the given file
Parser::Parser(const std::string& filename) : lexer_(filename) {
  // Start the lookahead with a newline token. This is a simple way to ensure
  // proper initialization, because the newline will essentially be ignored.
  lookahead_.reset(new AST::Node);
  *lookahead_ = AST::Node::newline_node();
}

// Parse the file with a shift-reduce algorithm
NPtr Parser::parse() {
  // The very first entry is a newline and should be shifted. Do that so we
  // can be certain the stack is never empty. This allows us to avoid checking
  // for the empty stack in the should_shift() method.
  shift();

  while (lookahead_->node_type() != Language::eof) {

    // Deterimine if you should shift or reduce. If you should shift, the
    // conditional will be true and the body will execute (and therefore
    // shift). Otherwise, the second part of the conditional will be executed,
    // and we will attempt to reduce the stream. If we successfully reduce,
    // the whole conditional will evaluate to false, and we will not execute
    // the body (i.e., we will not shift). On the other hand, if there was
    // no reduce possible, then the conditional will evaluate to true, and we
    // will shift.
    if (should_shift() || !reduce()) shift();

    // A flag given to the compiler that will tell it to pause at each
    // shift/reduce step and show the current parse stack.
    if (debug::parser) show_debug();
  }
 
  // Once we exit the previous loop, we have seen all tokens and reached the
  // end of the file. There cannot be any more shifting, but there may be more
  // reductions to complete. While we can reduce, do so.
  while (reduce()) {
    if (debug::parser) show_debug();
  }

  if (debug::parser) {
    std::cout << "========== Parsing complete ==========" << std::endl;
  }

  // It is impossible for the stack to be empty because the lookahead_ starts
  // with newline and we continue until it's equal to EOF. Thus, at least one
  // token is shifted onto the stack and each stack operation preserves the
  // non-empty invariant.
  //
  // TODO Make this impossible to be an issue
  if (stack_.size() > 1) {
    std::cerr
      << "Parser found several nodes at root level. Handling just the first tree."
      << std::endl;
  }

  return stack_.back();
}

// Print out the debug information for the parse stack, and pause.
void Parser::show_debug() const {
  // Clear the screen
  std::cout << "\033[2J\033[1;1H" << std::endl;
  for (const auto& node_ptr : stack_) {
    std::cout << *node_ptr;
  }
  std::cin.ignore(1);
}

// This function determines if a shift should be done, even when a valid
// reduce is possible. Recall that the stack can never be empty, so calls to
// stack_.back() are always safe.
bool Parser::should_shift() {
  // We'll need these node types a lot, so lets make it easy to use
  const auto last_type = stack_.back()->node_type();
  const auto ahead_type = lookahead_->node_type();

  // If we see an identifier followed by a decl_operator, shift. Without this
  // check present, an identifier would get changed to an expression, and the
  // verification that it really is something defineable would be more
  // computationally intensive.
  if (last_type == Language::identifier && Language::is_decl(ahead_type)) {
    return true;
  }

  if (stack_.size() >= 2
      && ahead_type == Language::newline
      && last_type == Language::string_literal
      && stack_[stack_.size() - 2]->node_type() == Language::reserved_import) {
    return true;
  }

  // Reduce terminals
  switch (last_type) {
    case Language::identifier:
    case Language::reserved_bool_literal:
    case Language::integer_literal:
    case Language::real_literal:
    case Language::character_literal:
    case Language::string_literal:
    case Language::type_literal:
    case Language::right_paren:
    case Language::right_bracket:
      return false;
    default:;
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
      (ahead_type == Language::left_paren || ahead_type == Language::left_bracket) &&
      // Must also guarantee that we don't have a '.' beforehand 
      (stack_.size() < 2 ||
       stack_[stack_.size() - 2]->node_type() != Language::dot)) {
    return true;
  }

  // If we have two competing operators, one on the stack and a binary operator
  // in the lookahead, then we must compare their precedence levels to decide
  // whether or not to shift. We only shift if the one on the stack has lower
  // precedence.
  if (Language::is_binary_operator(ahead_type) && stack_.size() >= 2
      && Language::is_operator(stack_[stack_.size() - 2]->node_type())) {

#if DEBUG
    // .at() is more expensive, and should only be used in debug mode
    auto lhs_prec = Language::op_prec.at(stack_[stack_.size() - 2]->token());
    auto rhs_prec = Language::op_prec.at(lookahead_->token());
#else
    auto lhs_prec = Language::op_prec[stack_[stack_.size() - 2]->token()];
    auto rhs_prec = Language::op_prec[lookahead_->token()];
#endif

    if (lhs_prec != rhs_prec) return lhs_prec < rhs_prec;

    auto associativity = lhs_prec & assoc_mask;

    // Non-associative operators in this situation are a parsing error,
    // because the lhs and rhs precedences are the same.
    // 
    // TODO figure out if we should exit early here. Is there any reasonable
    // way to continue?
    if (associativity == non_assoc) {
      std::cout << "!!!" << lhs_prec << std::endl;

      error_log.log(lookahead_->line_num(),
          "Non-associative operator found with no specified association. Maybe you forgot parentheses?");
    }

    // If the precedence levels are equal, we should shift right-associative
    // operators. Chain operators are built as if they were left-associative.
    return associativity == right_assoc;
  }

  // If we're defining a function with braces don't stop early.
  if ((last_type == Language::fn_expression || last_type == Language::reserved_type) && ahead_type == Language::left_brace) {
    return true;
  }

  return false;
}

// Reduces the stack according to the language rules spceified in Language.cpp.
// Returns true if a rule is matched and applied. Returns false otherwise.
bool Parser::reduce() {
  const Rule* matched_rule_ptr = nullptr;


  for (const Rule& rule : Language::rules) {
    // If we've already matched a rule, ignore rules of lower precedence.
    // Precedence is simply determined by the length of the match.
    if (matched_rule_ptr != nullptr &&
        matched_rule_ptr->size() > rule.size()) {
      continue;
    }

    if (rule.match(stack_)) {
#ifdef DEBUG
      // It should be impossible to match multiple rules with the same precedence
      // levels.
      if (matched_rule_ptr != nullptr &&
          rule.size() == matched_rule_ptr->size()) {
        std::cerr << "FATAL: Two rules matched with the same size" << std::endl;
      }
#endif

      // Extract a pointer to the rule. It's safe to take a pointer here, because
      // Language::rules is const.
      matched_rule_ptr = &rule;
    }
  }

  // If you make it to the end of the rules and still haven't matched, then
  // return false
  if (matched_rule_ptr == nullptr) return false;

  // Apply the rule
  matched_rule_ptr->apply(stack_);

  return true;
}
