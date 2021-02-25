#ifndef ICARUS_IR_VALUE_JUMP_H
#define ICARUS_IR_VALUE_JUMP_H

#include <iostream>

#include "base/extend.h"

namespace ir {
struct CompiledJump;

struct Jump : base::Extend<Jump, 1>::With<base::AbslHashExtension> {
  explicit constexpr Jump(CompiledJump const *jump = nullptr) : jump_(jump) {}

  friend std::ostream &operator<<(std::ostream &os, Jump j) {
    return os << "Jump(" << j.jump_ << ")";
  }

 private:
  friend base::EnableExtensions;
  friend CompiledJump;

  CompiledJump const *jump_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_JUMP_H
