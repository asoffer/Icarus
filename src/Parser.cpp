#include "Parser.h"
#include "AST.h"

Parser::Parser(const char* filename) : lexer_(filename) {
  // Start the lookahead with a newline token
  lookahead_.reset(new AST::Node);
  *lookahead_ = AST::Node::newline_node();
}

NPtr Parser::parse() {
  while (lookahead_->node_type() != Language::eof) {
    if (should_shift() || !reduce()) {
      shift();
    }

#if 0
    // Clear the screen
    // std::cout << "\033[2J\033[1;1H" << std::endl;
    for (const auto& node_ptr : stack_) {
      std::cout << *node_ptr;
    }
    std::cin.ignore(1);
#endif

  }
  
  // Finish up any more reductions that can be made
  while (reduce());

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

bool Parser::should_shift() {
  if (stack_.empty()) return true;

  // If we see an identifier followed by a decl_operator, shift
  if (stack_.back()->node_type() == Language::identifier
      && lookahead_->node_type() == Language::decl_operator) {
    return true;
  }

  // Reduce terminals
  switch (stack_.back()->node_type()) {
    case Language::identifier:
    case Language::integer_literal:
    case Language::real_literal:
    case Language::character_literal:
    case Language::string_literal:
    case Language::type_literal:
    case Language::right_paren:
      return false;
    default:;
  }

  // For function calls, shift the parentheses on
  if (Language::is_expression(stack_.back()->node_type())
      && lookahead_->node_type() == Language::left_paren) {
    return true;
  }

  if (Language::is_binary_operator(lookahead_->node_type())
      && stack_.size() >= 2
      && Language::is_operator(stack_[stack_.size() - 2]->node_type())) {

    auto lhs_prec = Language::op_prec.at(stack_[stack_.size() - 2]-> token());
    auto rhs_prec = Language::op_prec.at(lookahead_->token());

    if (lhs_prec != rhs_prec)
      return lhs_prec < rhs_prec;

    // Shift for right-associative
    return (lhs_prec & 3) == right_assoc;
  }

  // If we're defining a function with braces don't stop early.
  if (stack_.back()->node_type() == Language::fn_expression
      && lookahead_->node_type() == Language::left_brace) {
    return true;
  }

  return false;
}

bool Parser::reduce() {
  const Rule* matched_rule_ptr = nullptr;

  for (const Rule& rule : Language::rules) {
    // If we've already found a rule, ignore rules of lower precedence
    if (matched_rule_ptr != nullptr &&
        matched_rule_ptr->size() > rule.size()) {
      continue;
    }

    if (rule.match(stack_)) {
#ifdef DEBUG
      if (matched_rule_ptr != nullptr &&
          rule.size() == matched_rule_ptr->size()) {
        std::cerr << "FATAL: Two rules matched with the same size" << std::endl;
      }
#endif

      matched_rule_ptr = &rule;
    }
  }

  if (matched_rule_ptr == nullptr) return false;

  matched_rule_ptr->apply(stack_);

  return true;
}
