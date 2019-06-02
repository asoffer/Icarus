#ifndef ICARUS_BACKEND_EXEC_H
#define ICARUS_BACKEND_EXEC_H

#include <memory>
#include <cstddef>
#include <stack>

#include "base/untyped_buffer.h"
#include "ir/basic_block.h"
#include "ir/cmd.h"

namespace backend {
struct ExecContext {
  ExecContext();

  struct Frame {
    Frame() = delete;
    Frame(ir::CompiledFn *fn, const base::untyped_buffer &arguments);

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

  ir::BlockIndex ExecuteBlock(const std::vector<ir::Addr> &ret_slots);
  ir::BlockIndex ExecuteCmd(const ir::Cmd &cmd,
                            const std::vector<ir::Addr> &ret_slots);

  template <typename T>
  T resolve(ir::Reg val) const;

  template <typename T>
  T resolve(ir::RegisterOr<T> val) const {
    return val.is_reg_ ? resolve<T>(val.reg_) : val.val_;
  }

  base::untyped_buffer stack_;
};

void Execute(ir::CompiledFn *fn, const base::untyped_buffer &arguments,
             const std::vector<ir::Addr> &ret_slots, ExecContext *ctx);
}  // namespace backend
#endif  // ICARUS_BACKEND_EXEC_H
