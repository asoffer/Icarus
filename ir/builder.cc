#include "ir/builder.h"

#include <memory>

#include "ir/compiled_fn.h"

namespace ir {

thread_local Builder current;

Builder& GetBuilder() { return current; }

BasicBlock* Builder::AddBlock() {
  return current_.func_->blocks_
      .emplace_back(std::make_unique<BasicBlock>(current_.func_))
      .get();
}

SetCurrentFunc::SetCurrentFunc(CompiledFn* fn)
    : old_fn_(GetBuilder().current_.func_),
      old_block_(GetBuilder().CurrentBlock()) {
  GetBuilder().current_.func_  = fn;
  GetBuilder().current_.block_ = fn->entry();
}

SetCurrentFunc::~SetCurrentFunc() {
  GetBuilder().current_.func_ = old_fn_;
  GetBuilder().CurrentBlock() = old_block_;
}

}  // namespace ir
