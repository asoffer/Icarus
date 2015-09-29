#include "Parser.h"
#include "AST/Node.h"
#include "AST/Expression.h"
#include "AST/Terminal.h"
#include "AST/Identifier.h"
#include "AST/Binop.h"
#include "AST/KVPairList.h"
#include "AST/Case.h"
#include "AST/Scope.h"
#include "AST/AnonymousScope.h"
#include "AST/Statements.h"

template <size_t N> NPtr drop_all_but(NPtrVec&& nodes) {
  return std::move(nodes[N]);
}

Parser::Parser(const char* filename) : lexer_(filename) {
  init_rules();

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

//  for (const auto& node_ptr : stack_) {
//    std::cout << *node_ptr;
//  }
//  std::cout << std::endl;
//  std::string s;
//  std::cin >> s;
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
  if (stack_.back()->node_type() == AST::Node::identifier
      || stack_.back()->node_type() == AST::Node::integer
      || stack_.back()->node_type() == AST::Node::real
      || stack_.back()->node_type() == AST::Node::string_literal
      || stack_.back()->node_type() == AST::Node::right_paren) {
    return false;
  }

  // For function calls, shift the parentheses on
  if (stack_.back()->node_type() == AST::Node::expression
      && lookahead_->node_type() == AST::Node::left_paren) {
    return true;
  }

  if (lookahead_->node_type() == AST::Node::generic_operator
      && stack_.size() >= 2
      && stack_[stack_.size() - 2]->node_type() == AST::Node::generic_operator) {
    // TODO worry about associtavitiy
    return AST::prec_map[stack_[stack_.size() - 2]->token()] < AST::prec_map[lookahead_->token()];
  }

  return false;
}

bool Parser::reduce() {
  const Rule* matched_rule_ptr = nullptr;

  for (const Rule& rule : rules_) {
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

void Parser::init_rules() {
  using AST::Node;

  rules_.push_back(Rule(Node::expression, {
        Node::identifier
        }, AST::Identifier::build));

  rules_.push_back(Rule(Node::expression, {
        Node::integer
        }, AST::Terminal::build_integer));

  rules_.push_back(Rule(Node::expression, {
        Node::real
        }, AST::Terminal::build_real));

  rules_.push_back(Rule(Node::expression, {
        Node::string_literal
        }, AST::Terminal::build_string_literal));

  rules_.push_back(Rule(Node::expression, {
        Node::left_paren, Node::expression, Node::right_paren
        }, AST::Expression::parenthesize));

//  rules_.push_back(Rule(Node::expression, {
//        Node::identifier, Node::declaration, Node::expression, Node::assignment, Node::expression
//        }, ));

  rules_.push_back(Rule(Node::expression, {
        Node::expression, Node::generic_operator, Node::expression
        }, AST::Binop::build));

  rules_.push_back(Rule(Node::expression, {
        Node::expression, Node::left_paren, Node::expression, Node::right_paren
        }, AST::Binop::build_paren_operator));

  rules_.push_back(Rule(Node::expression, {
        Node::expression, Node::left_bracket, Node::expression, Node::right_bracket
        }, AST::Binop::build_bracket_operator));

  rules_.push_back(Rule(Node::key_value_pair, {
        Node::expression, Node::key_value_joiner, Node::expression, Node::newline
        }, AST::Binop::build));

  rules_.push_back(Rule(Node::key_value_pair, {
        Node::reserved_else, Node::key_value_joiner, Node::expression, Node::newline
        }, AST::Binop::build_else_kv));

  rules_.push_back(Rule(Node::key_value_pair, {
        Node::key_value_pair, Node::newline
        }, drop_all_but<0>));

  rules_.push_back(Rule(Node::key_value_pair_list, {
        Node::key_value_pair
        }, AST::KVPairList::build_one));

  rules_.push_back(Rule(Node::key_value_pair_list, {
        Node::key_value_pair_list, Node::newline
        }, drop_all_but<0>));

  rules_.push_back(Rule(Node::key_value_pair_list, {
        Node::key_value_pair_list, Node::key_value_pair
        }, AST::KVPairList::build_more));

  rules_.push_back(Rule(Node::expression, {
        Node::left_brace, Node::statements, Node::right_brace
        }, AST::AnonymousScope::build));

  rules_.push_back(Rule(Node::expression, {
        Node::reserved_case, Node::left_brace, Node::newline, Node::key_value_pair_list, Node::right_brace
        }, AST::Case::build));

  rules_.push_back(Rule(Node::statements, {
        Node::expression, Node::newline
        }, AST::Statements::build_one));

  rules_.push_back(Rule(Node::statements, {
        Node::statements, Node::expression, Node::newline
        }, AST::Statements::build_more));

  rules_.push_back(Rule(Node::newline, {
        Node::newline, Node::newline
        }, drop_all_but<0>));

  // Disregard blank lines surrounding statements
  rules_.push_back(Rule(Node::statements, {
        Node::statements, Node::newline
        }, drop_all_but<0>));

  rules_.push_back(Rule(Node::statements, {
        Node::newline, Node::statements
        }, drop_all_but<1>));
}
