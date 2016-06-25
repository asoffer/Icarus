#include "Parser.h"

#ifndef ICARUS_UNITY
#include "Rule.h"
#endif

namespace debug {
extern bool parser;
} // namespace debug

#include "GrammarRules.h"

// Parse the file with a shift-reduce algorithm
AST::Node *Parser::parse() {
  assert(lookahead_.node_type == Language::bof);

  // Any valid program will clean this up eventually. Therefore, shifting on the
  // bof will not hurt us. The benefit of shifting is that we have now  enforced
  // the invariant that the stack is never empty. This means we do not need to
  // check for an empty stack in the should_shift method.
  shift();

  while (lookahead_.node_type != Language::eof) {
    assert(node_type_stack_.size() == node_stack_.size());
    // Shift if you are supposed to, or if you are unable to reduce.
    if (should_shift() || !reduce()) { shift(); }

    if (debug::parser) { show_debug(); }
  }

  return cleanup();
}

AST::Node *Parser::cleanup() {
  while (reduce()) {
    if (debug::parser) { show_debug(); }
  }
  shift(); // Shift on the EOF token
  assert(get_type(1) == Language::eof);

  // TODO there is one is production using EOF, so this loop is overkill. This
  // is technically not true beacuse you have NEWLINE_OR_EOF. Think about this.
  while (reduce()) {
    if (debug::parser) { show_debug(); }
  }

  if (node_stack_.size() > 1) {
    error_log.log(TokenLocation(), "Parser error.");
  }

  return node_stack_.back();
}

// Print out the debug information for the parse stack, and pause.
void Parser::show_debug() const {
  // Clear the screen
  std::cout << "\033[2J\033[1;1H" << std::endl;
  for (auto x : node_type_stack_) {
    std::cout << x << ", ";
  }
  std::cout << std::endl;

  for (const auto &node_ptr : node_stack_) {
    std::cout << *node_ptr;
  }

  std::cin.ignore(1);
}

void Parser::ignore() {
  auto next = lexer_.Next();

  delete lookahead_.node;
  lookahead_ = next;
}

void Parser::shift() {
  auto next = lexer_.Next();

  // Never shift comments onto the stack
  if (next.node_type == Language::comment) {
    delete next.node;
    shift();
    return;
  }

  node_type_stack_.push_back(lookahead_.node_type);
  node_stack_.push_back(lookahead_.node);
  lookahead_ = next;
}

// Construct a parser for the given file
Parser::Parser(const std::string &filename) : lexer_(filename) {
  assert(node_stack_.empty() && node_type_stack_.empty());
  // Start the lookahead with a bof token. This is a simple way to ensure  proper
  // initialization, because the newline will essentially be ignored.
  lookahead_ = NNT(new AST::TokenNode(lexer_.cursor.Location()), Language::bof);
}

// Reduces the stack according to the language rules spceified in Language.cpp.
// Returns true if a rule is matched and applied. Returns false otherwise.
bool Parser::reduce() {
  const Rule *matched_rule_ptr = nullptr;
  size_t debug_counter         = 0;
  size_t debug_match           = 0;

  for (const Rule &rule : Language::Rules) {
    ++debug_counter;
    // If we've already matched a rule, ignore rules of lower precedence (higher
    // integer value). I.e., 0x00 is the highest precedence. 0xff is the lowest.
    if (matched_rule_ptr != nullptr && matched_rule_ptr->prec < rule.prec) {
      continue;
    }

    if (rule.match(node_type_stack_)) {
      if (!((matched_rule_ptr == nullptr ||
             rule.prec != matched_rule_ptr->prec))) {
        std::cout << debug_counter << ", " << debug_match << std::endl;
      }

      assert((matched_rule_ptr == nullptr ||
              rule.prec != matched_rule_ptr->prec) &&
             "Two rules matched with the same precedence");

      debug_match = debug_counter;
      // Extract a pointer to the rule. It's safe to take a pointer here,
      // because
      // Language::rules is const.
      matched_rule_ptr = &rule;
    }
  }

  // If you make it to the end of the rules and still haven't matched, then
  // return false
  if (matched_rule_ptr == nullptr) { return false; }

  matched_rule_ptr->apply(node_stack_, node_type_stack_);

  return true;
}

