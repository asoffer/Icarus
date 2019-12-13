#include "ir/block_group.h"

#include "core/arch.h"
#include "type/pointer.h"

namespace ir::internal {

BlockGroup::BlockGroup(
    core::FnParams<type::Typed<ast::Declaration const *>> params)
    : params_(std::move(params)), num_regs_(params_.size()) {
  // Ensure the existence of an entry block. The entry block marks itself as
  // incoming so it is never accidentally cleaned up.
  auto *b = AppendBlock();
  b->incoming_.insert(b);
}

Reg BlockGroup::Reserve(type::Type const *t) {
  auto arch = core::Interpretter();
  return Reserve(t->bytes(arch), t->alignment(arch));
}

Reg BlockGroup::Reserve(core::Bytes b, core::Alignment a) {
  Reg r(num_regs_);
  Reserve(r, b, a);
  return r;
}

void BlockGroup::Reserve(Reg r, core::Bytes b, core::Alignment a) {
  ++num_regs_;
}

Reg BlockGroup::Alloca(type::Type const *t) {
  Reg r = Reserve(type::Ptr(t));
  allocs_.allocate(ASSERT_NOT_NULL(t), r);
  return r;
}

std::ostream &operator<<(std::ostream &os, BlockGroup const &b) {
  for (size_t i = 0; i < b.blocks().size(); ++i) {
    os << "\n block #" << i << " (" << b.blocks()[i] << ")\n" << *b.blocks()[i];
  }
  return os;
}

}  // namespace ir::internal
