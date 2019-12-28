#include "ir/new_inliner.h"

namespace ir {

void Inliner::Inline(BasicBlock*& block) const {
  auto iter = blocks_.find(block);
  ASSERT(iter != blocks_.end());
  block = iter->second;
}

void Inliner::Inline(Reg& r) const {
  if (r.is_arg()) {
    r = Reg(r.arg_value() + register_offset_);
  } else {
    r = Reg(r.value() + register_offset_);
  }
}

}  // namespace ir
