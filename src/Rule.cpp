#include "Rule.h"

#define DELETE(ptr)                                                            \
  {                                                                            \
    delete ptr;                                                                \
    ptr = nullptr;                                                             \
  }

Rule::Rule(unsigned short preced, Language::NodeType output,
           const OptVec &input, fnptr fn)
    : prec(preced), output_(output), input_(input), fn_(fn) {}

// Determine if the back of the stack matches the rule
bool Rule::match(const std::vector<Language::NodeType> &node_type_stack) const {
  // The stack needs to be long enough to match.
  if (input_.size() > node_type_stack.size()) return false;

  size_t stack_index = node_type_stack.size() - 1;
  size_t rule_index  = input_.size() - 1;

  // Iterate through backwards and exit as soon as you see a node whose
  // type does not match the rule.
  for (size_t i = 0; i < input_.size(); ++i, --rule_index, --stack_index) {
    if ((input_[rule_index] & node_type_stack[stack_index]) == 0) {
      return false;
    }
  }

  // If you complete the loop, there is a match.
  return true;
}

void Rule::apply(NPtrVec &node_stack,
                 std::vector<Language::NodeType> &node_type_stack) const {
  // Make a vector for the rule function to take as input. It will begin with
  // size() shared_ptrs.
  NPtrVec nodes_to_reduce(size());

  // A rule's size cannot be empty, so the int value for i will always start at
  // a non-negative integer. We use an int so we can condition on i >= 0.
  // (unsigned values always satisfy that condition).
  for (int i = (int)size() - 1; i >= 0; --i) {
    // We need an unsigned value to index nodes_to_reduce. This is why we cast
    // back to size_t.
    nodes_to_reduce[(size_t)i] = std::move(node_stack.back());

    node_type_stack.pop_back();
    node_stack.pop_back();
  }

  auto new_ptr = fn_(std::move(nodes_to_reduce));

  for (auto ptr : nodes_to_reduce) { DELETE(ptr); }

  node_stack.push_back(std::move(new_ptr));
  node_type_stack.push_back(output_);
}
