#ifndef ICARUS_IR_BUILDER_H
#define ICARUS_IR_BUILDER_H

#include "base/debug.h"
#include "base/scope.h"
#include "ir/basic_block.h"

namespace ir {
struct CompiledFn;

struct Builder {
  BlockIndex AddBlock();

  CompiledFn* function() { return ASSERT_NOT_NULL(current_.func_); }
  BlockIndex& CurrentBlock() { return current_.index_; }

  ICARUS_PRIVATE
  friend struct SetCurrentFunc;

  struct State {
    CompiledFn* func_ = nullptr;
    BlockIndex index_;
  } current_;
};

Builder& GetBuilder();

struct SetCurrentFunc : public base::UseWithScope {
  SetCurrentFunc(CompiledFn* fn);
  ~SetCurrentFunc();

  ICARUS_PRIVATE
  CompiledFn* old_fn_;
  BlockIndex old_block_;
};

}  // namespace ir

#endif  // ICARUS_IR_BUILDER_H
