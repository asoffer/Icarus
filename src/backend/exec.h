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

// This is basically just a buffer. Why wrap it?
struct Stack {
  Stack() = delete;
  Stack(size_t cap) : buffer_(cap) {}

  template <typename T>
  T Load(size_t index) const {
    return buffer_.get<T>(index);
  }

  template <typename T>
  void Store(T val, size_t index) {
    buffer_.set(index, val);
  }

  IR::Addr Push(const type::Pointer *ptr);

private:
  base::untyped_buffer buffer_;
};

struct ExecContext {
  ExecContext();

  struct Frame {
    Frame() = delete;
    Frame(Func *fn, const base::untyped_buffer &arguments);

    void MoveTo(BlockIndex block_index) {
      ASSERT(block_index.value >= 0);
      prev_    = current_;
      current_ = block_index;
    }

    Func *fn_ = nullptr;
    BlockIndex current_;
    BlockIndex prev_;

    base::untyped_buffer regs_;
  };

  BasicBlock &current_block();

  std::stack<Frame> call_stack;

  BlockIndex ExecuteBlock();
  BlockIndex ExecuteCmd(const Cmd &cmd);

  template <typename T>
  T resolve(Register val) const;

  template <typename T>
  T resolve(RegisterOr<T> val) const {
    return val.is_reg_ ? resolve<T>(val.reg_) : val.val_;
  }

  Stack stack_;
};
} // namespace IR
#endif // ICARUS_BACKEND_EXEC_H
