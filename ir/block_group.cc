#include "ir/block_group.h"

#include "core/arch.h"
#include "type/pointer.h"

namespace ir::internal {

BlockGroup::BlockGroup(
    core::FnParams<type::Typed<ast::Declaration const *>> params)
    : params_(std::move(params)),
      num_regs_(params_.size()),
      num_args_(params_.size()) {
  // Ensure the existence of an entry block. The entry block marks itself as
  // incoming so it is never accidentally cleaned up.
  auto *b = AppendBlock();
  b->incoming_.insert(b);
}

Reg BlockGroup::Alloca(type::Type const *t) {
  Reg r = Reserve();
  allocs_.allocate(ASSERT_NOT_NULL(t), r);
  return r;
}

std::ostream &operator<<(std::ostream &os, BlockGroup const &b) {
  for (size_t i = 0; i < b.blocks().size(); ++i) {
    using base::stringify;
    os << "\n block #" << i << " (" << stringify(b.blocks()[i]) << ")\n"
       << *b.blocks()[i];
  }
  return os;
}

}  // namespace ir::internal
