#ifndef ICARUS_BACKEND_EXEC_H
#define ICARUS_BACKEND_EXEC_H
#include <cstddef>
#include <stack>

#include "base/untyped_buffer.h"
#include "ir/basic_block.h"
#include "ir/cmd.h"
#include "ir/val.h"

namespace IR {
struct Func;
}  // namespace IR

namespace backend {
struct ExecContext {
  ExecContext();

  struct Frame {
    Frame() = delete;
    Frame(IR::Func *fn, const base::untyped_buffer &arguments);

    void MoveTo(IR::BlockIndex block_index) {
      ASSERT(block_index.value >= 0);
      prev_    = current_;
      current_ = block_index;
    }

    IR::Func *fn_ = nullptr;
    IR::BlockIndex current_;
    IR::BlockIndex prev_;

    base::untyped_buffer regs_;
  };

  IR::BasicBlock &current_block();

  std::stack<Frame> call_stack;

  IR::BlockIndex ExecuteBlock(const base::vector<IR::Addr> &ret_slots);
  IR::BlockIndex ExecuteCmd(const IR::Cmd &cmd,
                            const base::vector<IR::Addr> &ret_slots);

  template <typename T>
  T resolve(IR::Register val) const;

  template <typename T>
  T resolve(IR::RegisterOr<T> val) const {
    return val.is_reg_ ? resolve<T>(val.reg_) : val.val_;
  }

  base::untyped_buffer stack_;
};

void Execute(IR::Func *fn, const base::untyped_buffer &arguments,
             const base::vector<IR::Addr> &ret_slots,
             backend::ExecContext *ctx);
}  // namespace backend
#endif  // ICARUS_BACKEND_EXEC_H
