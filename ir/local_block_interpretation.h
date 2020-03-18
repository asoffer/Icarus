#ifndef ICARUS_IR_LOCAL_BLOCK_INTERPRETATION_H
#define ICARUS_IR_LOCAL_BLOCK_INTERPRETATION_H

#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "base/macros.h"
#include "ir/blocks/basic.h"

namespace ir {
// LocalBlockInterpretation:
//
// Represents a mapping between blocks (i.e., targets of the `goto` keyword) and
// an an actual block local to a function (or internal::BlockGroupBase, more
// generally, because scopes such as `if` can just as easily be embedded in
// jumps.
struct LocalBlockInterpretation {
  explicit LocalBlockInterpretation(
      absl::flat_hash_map<ast::BlockNode const *, BasicBlock *> data,
      BasicBlock *starting_block, BasicBlock *landing_block)
      : data_(std::move(data)), start_(starting_block), exit_(landing_block) {
    for (auto[block_node, _] : data_) {
      name_to_node_.emplace(block_node->name(), block_node);
    }
  }
  BasicBlock *operator[](std::string_view name) const {
    if (name == "exit") { return exit_; }
    if (name == "start") { return start_; }
    ASSIGN_OR(return nullptr, auto const &node, block_node(name));
    return operator[](&node);
  }

  BasicBlock *operator[](ast::BlockNode const *node) const {
    auto iter = data_.find(node);
    if (iter == data_.end()) { return nullptr; }
    return iter->second;
  }

  ast::BlockNode const *block_node(std::string_view name) const {
    auto name_iter = name_to_node_.find(name);
    if (name_iter == name_to_node_.end()) { return nullptr; }
    return name_iter->second;
  }

 private:
  absl::flat_hash_map<ast::BlockNode const *, BasicBlock *> data_;
  absl::flat_hash_map<std::string_view, ast::BlockNode const *> name_to_node_;
  BasicBlock *start_;
  BasicBlock *exit_;
};

}  // namespace ir

#endif  // ICARUS_IR_LOCAL_BLOCK_INTERPRETATION_H
