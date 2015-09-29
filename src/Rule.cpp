#include "Rule.h"

Rule::Rule(Language::NodeType output,
    const std::vector<Language::NodeType>& input, fnptr fn)
: output_(output), input_(input), fn_(fn)
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

void Rule::apply(std::vector<NPtr>& node_stack) const {
  // Make a vector for the rule function to take as input. It will begin with
  // size() unique_ptrs.
  std::vector<NPtr> nodes_to_reduce(size());

  for (int i = static_cast<int>(size()) - 1; i >= 0; --i) {
    nodes_to_reduce[i] = std::move(node_stack.back());
    node_stack.pop_back();
  }

  auto new_ptr = fn_(std::move(nodes_to_reduce));
  new_ptr->set_node_type(output_);

  node_stack.push_back(std::move(new_ptr));
}
