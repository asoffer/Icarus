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
  using Language::NodeType;

  rules_.push_back(Rule(Language::expression, {
        Language::identifier
        }, AST::Identifier::build));

  rules_.push_back(Rule(Language::expression, {
        Language::integer
        }, AST::Terminal::build_integer));

  rules_.push_back(Rule(Language::expression, {
        Language::real
        }, AST::Terminal::build_real));

  rules_.push_back(Rule(Language::expression, {
        Language::string_literal
        }, AST::Terminal::build_string_literal));

  rules_.push_back(Rule(Language::expression, {
        Language::left_paren, Language::expression, Language::right_paren
        }, AST::Expression::parenthesize));

  //  rules_.push_back(Rule(Language::expression, {
  //        Language::identifier, Language::declaration, Language::expression, Language::assignment, Language::expression
  //        }, ));

  rules_.push_back(Rule(Language::expression, {
        Language::expression, Language::generic_operator, Language::expression
        }, AST::Binop::build));

  rules_.push_back(Rule(Language::expression, {
        Language::expression, Language::left_paren, Language::expression, Language::right_paren
        }, AST::Binop::build_paren_operator));

  rules_.push_back(Rule(Language::expression, {
        Language::expression, Language::left_bracket, Language::expression, Language::right_bracket
        }, AST::Binop::build_bracket_operator));

  rules_.push_back(Rule(Language::key_value_pair, {
        Language::expression, Language::key_value_joiner, Language::expression, Language::newline
        }, AST::Binop::build));

  rules_.push_back(Rule(Language::key_value_pair, {
        Language::reserved_else, Language::key_value_joiner, Language::expression, Language::newline
        }, AST::Binop::build_else_kv));

  rules_.push_back(Rule(Language::key_value_pair, {
        Language::key_value_pair, Language::newline
        }, drop_all_but<0>));

  rules_.push_back(Rule(Language::key_value_pair_list, {
        Language::key_value_pair
        }, AST::KVPairList::build_one));

  rules_.push_back(Rule(Language::key_value_pair_list, {
        Language::key_value_pair_list, Language::newline
        }, drop_all_but<0>));

  rules_.push_back(Rule(Language::key_value_pair_list, {
        Language::key_value_pair_list, Language::key_value_pair
        }, AST::KVPairList::build_more));

//  rules_.push_back(Rule(Language::expression, {
//        Language::left_brace, Language::statements, Language::right_brace
//        }, AST::AnonymousScope::build));

  rules_.push_back(Rule(Language::expression, {
        Language::reserved_case, Language::left_brace, Language::newline, Language::key_value_pair_list, Language::right_brace
        }, AST::Case::build));

  rules_.push_back(Rule(Language::statements, {
        Language::expression, Language::newline
        }, AST::Statements::build_one));

  rules_.push_back(Rule(Language::statements, {
        Language::statements, Language::expression, Language::newline
        }, AST::Statements::build_more));

  rules_.push_back(Rule(Language::newline, {
        Language::newline, Language::newline
        }, drop_all_but<0>));

  // Disregard blank lines surrounding statements
  rules_.push_back(Rule(Language::statements, {
        Language::statements, Language::newline
        }, drop_all_but<0>));

  rules_.push_back(Rule(Language::statements, {
        Language::newline, Language::statements
        }, drop_all_but<1>));
}
