#ifndef ICARUS_BACKEND_EXEC_H
#define ICARUS_BACKEND_EXEC_H

#include <cstddef>
#include <memory>
#include <stack>

#include "base/untyped_buffer.h"
#include "ir/addr.h"
#include "ir/block_def.h"
#include "ir/compiled_fn.h"
#include "ir/reg.h"
#include "ir/reg_or.h"

namespace ir {
struct BasicBlock;
struct ScopeDef;
}  // namespace ir

namespace backend {
struct RegisterArray {
  static constexpr size_t kMaxSize = 16;
  explicit RegisterArray(size_t num_regs)
      : data_(base::untyped_buffer::MakeFull(num_regs * kMaxSize)) {
    DEBUG_LOG("RegisterArray")(num_regs);
  }

  void write(base::untyped_buffer const &b) { data_.write(0, b); }

  auto raw(ir::Reg r) {
    if (r.is_arg()) NOT_YET();
    if (r.is_out()) NOT_YET();
    return data_.raw(r.value() * kMaxSize);
  }

  template <typename T>
  auto get(ir::Reg r) const {
    static_assert(sizeof(T) <= kMaxSize);
    if (r.is_arg()) NOT_YET();
    if (r.is_out()) NOT_YET();
    return data_.get<T>(r.value() * kMaxSize);
  }

  template <typename T>
  auto set(ir::Reg r, T const &val) {
    static_assert(sizeof(T) <= kMaxSize);
    if (r.is_arg()) NOT_YET();
    if (r.is_out()) NOT_YET();
    return data_.set<T>(r.value() * kMaxSize, val);
  }

 private:
  base::untyped_buffer data_;
};

struct ExecContext {
  ExecContext();

  struct Frame {
    Frame() = delete;
    Frame(ir::CompiledFn *fn, const base::untyped_buffer &arguments,
          ExecContext *ctx);

    void MoveTo(ir::BasicBlock const *block) {
      prev_    = current_;
      current_ = block;
    }

    ir::CompiledFn *fn_ = nullptr;
    ir::BasicBlock const *current_;
    ir::BasicBlock const *prev_;

    RegisterArray regs_;
  };

  ir::BasicBlock const *current_block();

  std::stack<Frame> call_stack;

  void ExecuteBlock(std::vector<ir::Addr> const &ret_slots);

  template <typename T>
  T resolve(ir::Reg r) const {
    return call_stack.top().regs_.get<T>(r);
  }

  template <typename T>
  T resolve(ir::RegOr<T> val) const {
    return val.resolve([&](ir::Reg r) { return resolve<T>(r); });
  }

  base::untyped_buffer stack_;
};

void Execute(ir::CompiledFn *fn, base::untyped_buffer const &arguments,
             std::vector<ir::Addr> const &ret_slots, ExecContext *ctx);
void Execute(ir::AnyFunc fn, base::untyped_buffer const &arguments,
             std::vector<ir::Addr> const &ret_slots, ExecContext *ctx);
}  // namespace backend
#endif  // ICARUS_BACKEND_EXEC_H
