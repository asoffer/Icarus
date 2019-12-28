#ifndef ICARUS_IR_BLOCK_H
#define ICARUS_IR_BLOCK_H

#include <iostream>

#include "base/debug.h"
#include "ir/any_func.h"

namespace type {
struct Jump;
}  // namespace type

namespace ir {
struct Jump;

struct BlockDef {
  static BlockDef const *Start();
  static BlockDef const *Exit();

  inline friend std::ostream &operator<<(std::ostream &os, BlockDef const &b) {
    return os << "blockdef{" << b.before_.size() << ", " << b.after_.size()
              << "}";
  }

  type::Jump const *type() const { return type_; }

  std::vector<AnyFunc> before_;
  std::vector<Jump *> after_;

 private:
  type::Jump const *type_ = nullptr;
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCK_H
