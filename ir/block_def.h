#ifndef ICARUS_IR_BLOCK_H
#define ICARUS_IR_BLOCK_H

#include <iostream>

#include "base/debug.h"
#include "ir/any_func.h"

namespace ir {
struct Jump;

struct BlockDef {
  static BlockDef const *Start();
  static BlockDef const *Exit();

  inline friend std::ostream &operator<<(std::ostream &os, BlockDef const &b) {
    return os << "blockdef{" << b.before_.size() << ", " << b.after_.size()
              << "}";
  }

  std::vector<AnyFunc> before_;
  std::vector<Jump const *> after_;
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCK_H
