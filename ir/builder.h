#ifndef ICARUS_IR_BUILDER_H
#define ICARUS_IR_BUILDER_H

#include "base/debug.h"
#include "base/scope.h"
#include "ir/basic_block.h"

namespace ir {
struct CompiledFn;

struct Builder {
  BasicBlock* AddBlock();

  CompiledFn* function() { return ASSERT_NOT_NULL(current_.func_); }
  BasicBlock*& CurrentBlock() { return current_.block_; }

  ICARUS_PRIVATE
  friend struct SetCurrentFunc;

  struct State {
    CompiledFn* func_ = nullptr;
    BasicBlock* block_;
  } current_;
};

Builder& GetBuilder();

struct SetCurrentFunc : public base::UseWithScope {
  SetCurrentFunc(CompiledFn* fn);
  ~SetCurrentFunc();

  ICARUS_PRIVATE
  CompiledFn* old_fn_;
  BasicBlock *old_block_;
};

}  // namespace ir

#endif  // ICARUS_IR_BUILDER_H
