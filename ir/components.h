#ifndef ICARUS_IR_COMPONENTS_H
#define ICARUS_IR_COMPONENTS_H

#include <tuple>

#include "base/tuple.h"
#include "ir/cmd.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "ir/register.h"

namespace ir {
template <bool B>
BlockIndex EarlyExitOn(BlockIndex exit_block, RegisterOr<bool> cond) {
  auto continue_block = Func::Current->AddBlock();
  if constexpr (B) {
    CondJump(cond, exit_block, continue_block);
  } else {
    CondJump(cond, continue_block, exit_block);
  }
  return continue_block;
}

inline Register PtrFix(Register r, type::Type const *desired_type) {
  return desired_type->is_big() ? r : Load(r, desired_type);
}
}  // namespace ir

#endif  // ICARUS_IR_COMPONENTS_H
