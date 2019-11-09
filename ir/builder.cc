#include "ir/builder.h"

#include <memory>

#include "ir/block_group.h"

namespace ir {

thread_local Builder current;

Builder& GetBuilder() { return current; }

BasicBlock* Builder::AddBlock() { return CurrentGroup()->AppendBlock(); }

SetCurrentFunc::SetCurrentFunc(internal::BlockGroup* group)
    : old_group_(GetBuilder().CurrentGroup()),
      old_block_(GetBuilder().CurrentBlock()) {
  GetBuilder().CurrentGroup()  = group;
  GetBuilder().current_.block_ = group->entry();
}

SetCurrentFunc::~SetCurrentFunc() {
  GetBuilder().CurrentGroup() = old_group_;
  GetBuilder().CurrentBlock() = old_block_;
}

base::Tagged<Addr, Reg> Builder::Alloca(type::Type const* t) {
  return CurrentGroup()->Alloca(t);
}

base::Tagged<Addr, Reg> Builder::TmpAlloca(type::Type const* t) {
  auto reg = Alloca(t);
  current_.temporaries_to_destroy_.emplace_back(reg, t);
  return reg;
}

Reg Reserve(core::Bytes b, core::Alignment a) {
  return current.CurrentGroup()->Reserve(b, a);
}

Reg Reserve(type::Type const* t) { return current.CurrentGroup()->Reserve(t); }

}  // namespace ir
