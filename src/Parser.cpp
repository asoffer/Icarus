#include "Parser.h"
#include "AST/Node.h"

Parser::Parser(const char* filename) : lexer_(filename) {
  lookahead_ = NPtr(new AST::Node);
  *lookahead_ = AST::Node::newline_node();
}

NPtr Parser::parse() {
  while (lexer_) {
    if (should_shift()) {
      shift();
    } else if (!reduce()) {
      shift();
    }

//      for (const auto& node_ptr : stack_) {
//        std::cout << *node_ptr;
//      }
//      std::cout << std::endl;
//      std::string s;
//      std::cin >> s;
  }
  // Finish up any more reductions that can be made
  while (reduce());

  for (const auto& node_ptr : stack_) {
    std::cout << *node_ptr;
  }
  std::cout << std::endl;

  // FIXME does it exist? is there only one?
  NPtr back = std::move(stack_.back());
  stack_.pop_back();
  return back;
}

bool Parser::should_shift() {
  // Shift if the stack is empty
  if (stack_.size() == 0) return true;

  // Reduce terminals
  if (stack_.back()->node_type() == Language::identifier
      || stack_.back()->node_type() == Language::integer
      || stack_.back()->node_type() == Language::real
      || stack_.back()->node_type() == Language::string_literal
      || stack_.back()->node_type() == Language::right_paren) {
    return false;
  }

  // For function calls, shift the parentheses on
  if (stack_.back()->node_type() == Language::expression
      && lookahead_->node_type() == Language::left_paren) {
    return true;
  }

  if (lookahead_->node_type() == Language::generic_operator
      && stack_.size() >= 2
      && stack_[stack_.size() - 2]->node_type() == Language::generic_operator) {
    // TODO worry about associtavitiy

    return Language::op_prec.at(stack_[stack_.size() - 2]->token()) < Language::op_prec.at(lookahead_->token());
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
        throw "Two rules matched with the same size";
      }
#endif

      matched_rule_ptr = &rule;
    }
  }

  if (matched_rule_ptr == nullptr) return false;

  matched_rule_ptr->apply(stack_);
  return true;
}
