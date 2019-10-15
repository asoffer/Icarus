#ifndef ICARUS_IR_BLOCK_H
#define ICARUS_IR_BLOCK_H

#include <iostream>

#include "base/debug.h"
#include "ir/any_func.h"

namespace ir {

struct BlockDef {
  explicit BlockDef(std::vector<AnyFunc> before, std::vector<AnyFunc> after)
      : before_(std::move(before)), after_(std::move(after)) {}

  static BlockDef const *Start();
  static BlockDef const *Exit();

  inline friend std::ostream &operator<<(std::ostream &os, BlockDef const &b) {
    return os << "blockdef{" << b.before_.size() << ", " << b.after_.size()
              << "}";
  }

  std::vector<AnyFunc> before_, after_;
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCK_H
