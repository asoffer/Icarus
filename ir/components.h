#ifndef ICARUS_IR_COMPONENTS_H
#define ICARUS_IR_COMPONENTS_H

#include "ir/func.h"
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

// TODO you're passing the types because you're not sure you can trust the one
// stored in the val. Is it a pointer on big types? You should probably get rid
// of it entirely, of start trusting it.
RegisterOr<bool> EmitEq(type::Type const *lhs_type, ir::Val const &lhs_val,
                        type::Type const *rhs_type, ir::Val const &rhs_val);

}  // namespace ir

#endif  // ICARUS_IR_COMPONENTS_H
