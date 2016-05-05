#include "Parser.h"
#include "Rule.h"

namespace debug {
extern bool parser;
} // namespace debug

#include "GrammarRules.h"

// Parse the file with a shift-reduce algorithm
AST::Node *Parser::parse() {
  assert(lookahead_->node_type() == Language::bof);

  // Any valid program will clean this up eventually. Therefore, shifting on the
  // bof will not hurt us. The benefit of shifting is that we have now  enforced
  // the invariant that the stack is never empty. This means we do not need to
  // check for an empty stack in the should_shift method.
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
  shift(); // Shift on the EOF token
  assert(get_type(1) == Language::eof);

  // TODO there is one is production using EOF, so this loop is overkill. This
  // is technically not true beacuse you have NEWLINE_OR_EOF. Think about this.
  while (reduce()) {
    if (debug::parser) { show_debug(); }
  }

  if (stack_.size() > 1) {
    if (debug::parser) {
      std::cerr << "Parser error: Exiting with Stack size = " << stack_.size()
                << std::endl;
    }
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
  // Start the lookahead with a boftoken. This is a simple way to ensure  proper
  // initialization, because the newline will essentially be ignored.
  lookahead_ = new AST::TokenNode(TokenLocation(), Language::bof);
}

// Reduces the stack according to the language rules spceified in Language.cpp.
// Returns true if a rule is matched and applied. Returns false otherwise.
bool Parser::reduce() {
  const Rule *matched_rule_ptr = nullptr;
  for (const Rule &rule : Language::Rules) {
    // If we've already matched a rule, ignore rules of lower precedence (higher
    // integer value). I.e., 0x00 is the highest precedence. 0xff is the lowest.
    if (matched_rule_ptr != nullptr && matched_rule_ptr->prec < rule.prec) {
      continue;
    }

    if (rule.match(stack_)) {
      assert((matched_rule_ptr == nullptr ||
              rule.prec != matched_rule_ptr->prec) &&
             "Two rules matched with the same precedence");

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

