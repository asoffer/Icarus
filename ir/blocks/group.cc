#include "ir/blocks/group.h"

namespace ir::internal {

BlockGroupBase::BlockGroupBase(core::Params<type::QualType> params,
                               size_t num_state_args)
    : params_(std::move(params)), alloc_(params_.size() + num_state_args) {
  AppendBlock(BasicBlock::DebugInfo{.header = "Entry"});
}

Reg BlockGroupBase::Alloca(type::Type t) { return alloc_.StackAllocate(t); }
Reg BlockGroupBase::Alloca(core::TypeContour tc) {
  return alloc_.StackAllocate(tc);
}

std::ostream &operator<<(std::ostream &os, BlockGroupBase const &b) {
  os << "\n" << b.alloc_;
  for (size_t i = 0; i < b.blocks().size(); ++i) {
    os << "\n block #" << i << " (" << b.blocks()[i] << ")\n" << *b.blocks()[i];
  }
  return os;
}

}  // namespace ir::internal
