#ifndef ICARUS_IR_EXEC_H
#define ICARUS_IR_EXEC_H
#include <cstddef>
#include <stack>

#include "val.h"
#include "cmd.h"
#include "block.h"

namespace IR {
struct Func;

struct Stack {
  Stack() = delete;
  Stack(size_t cap) : capacity_(cap), stack_(calloc(1, capacity_)) {}
  Stack(const Stack &) = delete;
  Stack(Stack &&other) {
    free(stack_);
    stack_          = other.stack_;
    other.stack_    = nullptr;
    other.capacity_ = other.size_ = 0;
  }
  ~Stack() {
    for (const auto &dtor : dtors_) { dtor(); }
    free(stack_);
  }

  template <typename T> T Load(size_t index) {
    ASSERT_EQ(index & (alignof(T) - 1), 0u); // Alignment error
    if constexpr (std::is_trivially_default_constructible_v<T>) {
      return *reinterpret_cast<T *>(this->location(index));
    } else {
      T *ptr = *reinterpret_cast<T **>(this->location(index));
      if (ptr == nullptr) {
        ptr = new T;
        dtors_.emplace_back([this, index]() {
          delete *reinterpret_cast<T **>(this->location(index));
        });
      }
      return *ptr;
    }
  }

  template <typename T> void Store(T val, size_t index) {
    if constexpr (std::is_trivially_default_constructible_v<T>) {
      *reinterpret_cast<T *>(this->location(index)) = val;
    } else {
      delete *reinterpret_cast<T **>(this->location(index));
      *reinterpret_cast<T **>(this->location(index)) = new T(val);
    }
  }

  IR::Val Push(const Pointer *ptr);

  std::vector<std::function<void()>> dtors_;
  size_t capacity_ = 0;
  size_t size_     = 0;
  void *stack_     = nullptr;

private:
  void *location(size_t index) {
    ASSERT_LT(index, capacity_);
    return reinterpret_cast<void *>(reinterpret_cast<char *>(stack_) + index);
  }
};

struct ExecContext {
  ExecContext();

  struct Frame {
    Frame() = delete;
    Frame(Func *fn, const std::vector<Val> &arguments);

    void MoveTo(BlockIndex block_index) {
      ASSERT_GE(block_index.value, 0);
      prev_    = current_;
      current_ = block_index;
    }

    Func *fn_ = nullptr;
    BlockIndex current_;
    BlockIndex prev_;

    std::vector<Val> regs_ = {};
    std::vector<Val> rets_ = {};
  };

  Block &current_block();

  std::stack<Frame> call_stack;

  BlockIndex ExecuteBlock();
  Val ExecuteCmd(const Cmd &cmd);
  void Resolve(Val *v) const;


  Val reg(Register r) const {
    ASSERT_GE(r.value, 0);
    return call_stack.top().regs_ AT(static_cast<u32>(r.value));
  }
  Val &reg(Register r) {
    ASSERT_GE(r.value, 0);
    return call_stack.top().regs_[static_cast<u32>(r.value)];
  }

  Stack stack_;
};
} // namespace IR
#endif // ICARUS_IR_EXEC_H
