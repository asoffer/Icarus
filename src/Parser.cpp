#include "Parser.h"
#include "AST.h"
#include "ErrorLog.h"

extern ErrorLog error_log;

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
    case Mode::Good: {
      // Shift if you are supposed to, or if you are unable to reduce.
      if (should_shift() || !reduce()) { shift(); }
    } break;
    case Mode::BadLine:
    case Mode::BadBlock:
    case Mode::BadFile:
    case Mode::Done: return cleanup();
    }

    if (debug::parser) { show_debug(); }

    if (lookahead_->node_type() == Language::eof) { mode_ = Mode::Done; }
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
    error_log.log(0, "Parser error.");
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

void Parser::shift() {
  std::unique_ptr<AST::TokenNode> next_node_ptr(new AST::TokenNode);
  lexer_ >> *next_node_ptr;

  // Never shift comments onto the stack
  if (next_node_ptr->node_type() == Language::comment) {

    shift();
    return;
  }

  stack_.push_back(lookahead_.release());
  lookahead_ = std::move(next_node_ptr);
}

// Construct a parser for the given file
Parser::Parser(const std::string &filename)
    : lexer_(filename), mode_(Mode::Good) {
  assert(stack_.empty());
  // Start the lookahead with a newline token. This is a simple way to ensure
  // proper initialization, because the newline will essentially be ignored.
  lookahead_.reset(new AST::TokenNode);
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
  case Language::reserved_true:
  case Language::reserved_false:
  case Language::int_literal:
  case Language::uint_literal:
  case Language::real_literal:
  case Language::char_literal:
  case Language::string_literal:
  case Language::type_literal:
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
      error_log.log(lookahead_->line_num, "Non-associative operator found "
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
  if (matched_rule_ptr == nullptr)
    return false;

  // Apply the rule
  matched_rule_ptr->apply(stack_);

  return true;
}
