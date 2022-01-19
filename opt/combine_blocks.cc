#include "opt/combine_blocks.h"

#include <queue>
#include <type_traits>
#include <utility>

#include "base/log.h"
#include "ir/subroutine.h"
#include "ir/instruction/core.h"

namespace opt {

// TODO: Implement.
void ReduceEmptyBlocks(ir::Subroutine* fn) {}
void CombineBlocks(ir::Subroutine* fn) {}

void RemoveTrivialFunctionCalls(ir::Subroutine* fn) {
  for (auto& block : fn->mutable_blocks()) {
    for (auto const& inst : block->instructions()) {
      if (auto* call_inst = inst.if_as<ir::CallInstruction>()) {
        if (call_inst->func().is_reg()) { continue; }
        if (call_inst->func().value().kind() != ir::Fn::Kind::Native) {
          continue;
        }
        auto called_fn = call_inst->func().value().native();
        if (called_fn.type() != type::Func({}, {})) { continue; }
        // TODO track this as you go.
        for (auto const* called_block : called_fn->blocks()) {
          // What if it's null?
          if (not called_block->instructions().empty()) { goto next_inst; }
        }
        // inst = nullptr;
      }
    next_inst:;
    }
  }
}

}  // namespace opt
