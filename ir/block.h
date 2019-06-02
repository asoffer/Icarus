#ifndef ICARUS_IR_BLOCK_H
#define ICARUS_IR_BLOCK_H

#include <iostream>

#include "base/debug.h"
#include "ir/any_func.h"

namespace ast {
struct BlockLiteral;
}  // namespace ast

namespace ir {

struct BlockDef {
  explicit BlockDef() = default;
  explicit BlockDef(ast::BlockLiteral const *parent) : parent_(parent) {}

  static BlockDef const *Start();
  static BlockDef const *Exit();

  inline friend std::ostream &operator<<(std::ostream &os, BlockDef const& b) {
    return os << "blockdef{" << b.before_.size() << ", " << b.after_.size()
              << "}";
  }

  void AddBefore(AnyFunc f) { before_.push_back(f); }
  void AddAfter(AnyFunc f) { after_.push_back(f); }

  ast::BlockLiteral const *parent_;
  std::vector<AnyFunc> before_, after_;
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCK_H
