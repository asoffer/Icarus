#include "Rule.h"

Rule::Rule(
    AST::Node::Type output,
    const std::vector<AST::Node::Type>& input)
: output_(output), input_(input)
{

}

bool Rule::match(const std::vector<NPtr>& node_stack) const {
  if (input_.size() > node_stack.size()) return false;

  size_t stack_index = node_stack.size() - 1;
  size_t rule_index = input_.size() - 1;

  for (size_t i = 0; i < input_.size();
      ++i, --rule_index, --stack_index) {

    if (node_stack[stack_index]->node_type() != input_[rule_index]) {
      return false;
    }
  }

  return true;
}

void Rule::apply(std::vector<NPtr>&/* node_stack */) const {
  // TODO(andy) implement this
  // node_stack commented out to avoid -Werror=unused-parameter annoyances
}
