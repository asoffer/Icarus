#ifndef ICARUS_IR_BLOCK_H
#define ICARUS_IR_BLOCK_H

#include <iostream>

#include "base/debug.h"
#include "ir/value/overload_set.h"

namespace type {
struct Jump;
}  // namespace type

namespace ir {
struct Jump;

struct BlockDef {
  inline friend std::ostream &operator<<(std::ostream &os, BlockDef const &b) {
    return os << "blockdef";
  }

  type::Jump const *type() const { return type_; }

  ir::OverloadSet before_;
  std::vector<Jump *> after_;

 private:
  type::Jump const *type_ = nullptr;
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCK_H
