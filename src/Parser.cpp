#include "Parser.h"

Parser::Parser(const char* filename) : lexer_(filename) {}

void Parser::parse() {
  AST::Node node;
  while (lexer_ >> node) {
    std::cout << node << std::endl;
  }

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
      if (rule.precedence() == matched_rule_ptr->precedence()) {
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
