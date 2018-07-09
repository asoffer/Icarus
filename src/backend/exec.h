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
  T Load(size_t index) {
    return buffer_.get<T>(index);
  }

  template <typename T>
  void Store(T val, size_t index) {
    buffer_.set(index, val);
  }

  IR::Val Push(const type::Pointer *ptr);

private:
  base::untyped_buffer buffer_;
};

struct ExecContext {
  ExecContext();

  struct Frame {
    Frame() = delete;
    Frame(Func *fn, const base::vector<Val> &arguments);

    void MoveTo(BlockIndex block_index) {
      ASSERT(block_index.value >= 0);
      prev_    = current_;
      current_ = block_index;
    }

    Func *fn_ = nullptr;
    BlockIndex current_;
    BlockIndex prev_;

    base::vector<Val> regs_ = {};
  };

  BasicBlock &current_block();

  std::stack<Frame> call_stack;

  BlockIndex ExecuteBlock();
  Val ExecuteCmd(const Cmd &cmd);
  void Resolve(Val *v) const;


  Val reg(Register r) const {
    ASSERT(r.value >= 0);
    return call_stack.top().regs_.at(static_cast<u32>(r.value));
  }
  Val &reg(Register r) {
    ASSERT(r.value >= 0);
    return call_stack.top().regs_.at(static_cast<u32>(r.value));
  }

  Stack stack_;
};
} // namespace IR
#endif // ICARUS_BACKEND_EXEC_H
