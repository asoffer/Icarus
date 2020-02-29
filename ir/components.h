#ifndef ICARUS_IR_COMPONENTS_H
#define ICARUS_IR_COMPONENTS_H

#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/value/reg.h"

namespace ir {
base::Tagged<Addr, Reg> Index(type::Pointer const *t, Reg array_ptr,
                              RegOr<int64_t> offset);

template <bool B>
BasicBlock *EarlyExitOn(BasicBlock *exit_block, RegOr<bool> cond) {
  auto *continue_block = GetBuilder().AddBlock();
  if constexpr (B) {
    GetBuilder().CondJump(cond, exit_block, continue_block);
  } else {
    GetBuilder().CondJump(cond, continue_block, exit_block);
  }
  return continue_block;
}

inline Reg PtrFix(Reg r, type::Type const *desired_type) {
  return desired_type->is_big() ? r : Load(r, desired_type);
}

}  // namespace ir

#endif  // ICARUS_IR_COMPONENTS_H
