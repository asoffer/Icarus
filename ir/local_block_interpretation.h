#ifndef ICARUS_IR_LOCAL_BLOCK_INTERPRETATION_H
#define ICARUS_IR_LOCAL_BLOCK_INTERPRETATION_H

#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "ir/basic_block.h"

namespace ir {
// LocalBlockInterpretation:
//
// Represents a mapping between blocks (i.e., targets of the `goto` keyword) and
// an an actual block local to a function (or internal::BlockGroup, more
// generally, because scopes such as `if` can just as easily be embedded in
// jumps.
struct LocalBlockInterpretation {
  explicit LocalBlockInterpretation(
      absl::flat_hash_map<ast::BlockNode const *, BasicBlock *> data)
      : data_(std::move(data)) {}

  BasicBlock *operator[](ast::BlockNode const *node) const {
    auto iter = data_.find(node);
    if (iter == data_.end()) { return nullptr; }
    return iter->second;
  }

 private:
  absl::flat_hash_map<ast::BlockNode const *, BasicBlock *> data_;
};

}  // namespace ir

#endif // ICARUS_IR_LOCAL_BLOCK_INTERPRETATION_H
