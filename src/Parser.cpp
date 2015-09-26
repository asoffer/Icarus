#include "Parser.h"

Parser::Parser(const char* filename) : lexer_(filename) {
  init_rules();
}

void Parser::parse() {
  while (lexer_) {
    // Reduce if you can
    while (reduce());

    // Otherwise shift
    shift();
  }
}

bool Parser::reduce() {
  const Rule* matched_rule_ptr = nullptr;

  for (const Rule& rule : rules_) {
    // If we've already found a rule, ignore rules of lower precedence
    if (matched_rule_ptr != nullptr &&
        matched_rule_ptr->precedence() > rule.precedence()) {
      continue;
    }


    if (rule.match(stack_)) {
#ifdef DEBUG
      if (matched_rule_ptr != nullptr &&
          rule.precedence() == matched_rule_ptr->precedence()) {
        throw "Two rules matched with the same precedence level";
      }
#endif

      matched_rule_ptr = &rule;
    }
  }

  if (matched_rule_ptr == nullptr) return false;

  matched_rule_ptr->apply(stack_);

  return true;
}

void Parser::init_rules() {
  using AST::Node;

//  rules_.push_back(Rule(Node::expression, {
//        Node::identifier
//        }, 0, AST::Expression::from_identifier));
}
