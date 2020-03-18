#include "ir/blocks/group.h"

#include "core/arch.h"
#include "type/pointer.h"

namespace ir::internal {

BlockGroupBase::BlockGroupBase(
    core::Params<type::Typed<ast::Declaration const *>> params,
    size_t num_state_args)
    : params_(std::move(params)),
      num_regs_(params_.size() + num_state_args),
      num_args_(params_.size() + num_state_args) {
  // TODO is this still true with variadics?
  ASSERT(params().size() == fn_type->params().size());

  // Ensure the existence of an entry block. The entry block marks itself as
  // incoming so it is never accidentally cleaned up.
  auto *b = AppendBlock();
  b->insert_incoming(b);
}

Reg BlockGroupBase::Alloca(type::Type const *t) {
  Reg r = Reserve();
  allocs_.allocate(ASSERT_NOT_NULL(t), r);
  return r;
}

std::ostream &operator<<(std::ostream &os, BlockGroupBase const &b) {
  os << "\n" << b.allocs();
  for (size_t i = 0; i < b.blocks().size(); ++i) {
    using base::stringify;
    os << "\n block #" << i << " (" << stringify(b.blocks()[i]) << ")\n"
       << *b.blocks()[i];
  }
  return os;
}

}  // namespace ir::internal
