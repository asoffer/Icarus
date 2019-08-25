#ifndef ICARUS_BACKEND_EXEC_H
#define ICARUS_BACKEND_EXEC_H

#include <memory>
#include <cstddef>
#include <stack>

#include "base/untyped_buffer.h"
#include "ir/addr.h"
#include "ir/block.h"
#include "ir/compiled_fn.h"
#include "ir/reg.h"
#include "ir/register.h"

namespace ir {
struct Cmd;
struct BasicBlock;
struct ScopeDef;
}  // namespace ir

namespace backend {
struct ExecContext {
  ExecContext();

  struct Frame {
    Frame() = delete;
    Frame(ir::CompiledFn *fn, const base::untyped_buffer &arguments,
          ExecContext *ctx);

    void MoveTo(ir::BlockIndex block_index) {
      ASSERT(block_index.value >= 0);
      prev_    = current_;
      current_ = block_index;
    }

    ir::CompiledFn *fn_ = nullptr;
    ir::BlockIndex current_;
    ir::BlockIndex prev_;

    std::stack<ir::ScopeDef *> scope_defs_;
    std::stack<ir::BlockDef> block_defs_;

    base::untyped_buffer regs_;
  };

  ir::BasicBlock &current_block();

  std::stack<Frame> call_stack;

  ir::BlockIndex ExecuteBlock(std::vector<ir::Addr> const &ret_slots);
  ir::BlockIndex ExecuteCmd(ir::Cmd const &cmd,
                            std::vector<ir::Addr> const &ret_slots);

  template <typename T>
  T resolve(ir::Reg r) const {
    auto iter = call_stack.top().fn_->compiler_reg_to_offset_.find(r);
#if defined(ICARUS_DEBUG)
    if (iter == call_stack.top().fn_->compiler_reg_to_offset_.end()) {
      UNREACHABLE("Failed to find ", r, " in ",
                  call_stack.top().fn_->compiler_reg_to_offset_);
    }
#endif
    return call_stack.top().regs_.get<T>(iter->second);
  }

  template <typename T>
  T resolve(ir::RegOr<T> val) const {
    return val.is_reg_ ? resolve<T>(val.reg_) : val.val_;
  }

  base::untyped_buffer stack_;
};

void Execute(ir::CompiledFn *fn, base::untyped_buffer const &arguments,
             std::vector<ir::Addr> const &ret_slots, ExecContext *ctx);
void Execute(ir::AnyFunc fn, base::untyped_buffer const &arguments,
             std::vector<ir::Addr> const &ret_slots, ExecContext *ctx);
}  // namespace backend
#endif  // ICARUS_BACKEND_EXEC_H
