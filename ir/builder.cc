#include "ir/builder.h"
#include "ir/compiled_fn.h"

namespace ir {

thread_local Builder current;

Builder& GetBuilder() { return current; }

BlockIndex Builder::AddBlock() {
  BlockIndex index;
  index.value =
      static_cast<decltype(index.value)>(current_.func_->blocks_.size());
  current_.func_->blocks_.emplace_back(current_.func_);
  return index;
}

SetCurrentFunc::SetCurrentFunc(CompiledFn* fn)
    : old_fn_(GetBuilder().current_.func_), old_block_(GetBuilder().CurrentBlock()) {
  GetBuilder().current_.func_  = fn;
  GetBuilder().current_.index_ = fn->entry();
}

SetCurrentFunc::~SetCurrentFunc() {
  GetBuilder().current_.func_ = old_fn_;
  GetBuilder().CurrentBlock() = old_block_;
}

}  // namespace ir
