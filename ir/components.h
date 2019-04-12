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

inline Reg PtrFix(Reg r, type::Type const *desired_type) {
  return desired_type->is_big() ? r : Load(r, desired_type);
}

// TODO: Typed<Results>
RegisterOr<bool> EmitEq(type::Type const *lhs_type, ir::Results const &lhs_val,
                        type::Type const *rhs_type, ir::Results const &rhs_val);

}  // namespace ir

#endif  // ICARUS_IR_COMPONENTS_H
