#include "frontend/parse_rule.h"

#include "ast/node.h"

namespace frontend {

bool ParseRule::Match(absl::Span<Tag const> tag_stack) const {
  // The stack needs to be long enough to match.
  if (input_.size() > tag_stack.size()) { return false; }

  size_t stack_index = tag_stack.size() - 1;
  size_t rule_index  = input_.size() - 1;

  // Iterate through backwards and exit as soon as you see a node whose
  // type does not match the rule.
  for (size_t i = 0; i < input_.size(); ++i, --rule_index, --stack_index) {
    if ((input_[rule_index] & tag_stack[stack_index]) == 0) { return false; }
  }

  // If you complete the loop, there is a match.
  return true;
}

void ParseRule::Apply(std::vector<std::unique_ptr<ast::Node>> *node_stack,
                      std::vector<Tag> *tag_stack, Module *mod,
                      error::Log *error_log) const {
  auto nodes_to_reduce = absl::MakeSpan(
      std::addressof(*(node_stack->end() - this->size())), this->size());
  auto result = fn_(nodes_to_reduce, mod, error_log);
  tag_stack->resize(node_stack->size() - this->size());
  node_stack->resize(node_stack->size() - this->size());

  node_stack->push_back(std::move(result));
  tag_stack->push_back(output_);
}

}  // namespace frontend
