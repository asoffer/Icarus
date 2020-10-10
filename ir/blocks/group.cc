#include "ir/blocks/group.h"

#include "ast/ast.h"
#include "core/arch.h"
#include "type/pointer.h"

namespace ir::internal {

size_t NumNonConstants(
    core::Params<type::Typed<ast::Declaration const *>> const &params) {
  size_t num = 0;
  for (auto const &param : params) {
    if (param.value.get() and
        not(param.value.get()->flags() & ast::Declaration::f_IsConst)) {
      ++num;
    }
  }
  return num;
}

BlockGroupBase::BlockGroupBase(
    core::Params<type::Typed<ast::Declaration const *>> params,
    size_t num_state_args)
    : params_(std::move(params)),
      alloc_(NumNonConstants(params_) + num_state_args) {
  // Ensure the existence of an entry block. The entry block marks itself as
  // incoming so it is never accidentally cleaned up.
  auto *b = AppendBlock(BasicBlock::DebugInfo{.header = "Entry"});
  b->insert_incoming(b);
}

Reg BlockGroupBase::Alloca(type::Type const *t) {
  return alloc_.StackAllocate(ASSERT_NOT_NULL(t));
}

std::ostream &operator<<(std::ostream &os, BlockGroupBase const &b) {
  os << "\n" << b.alloc_;
  for (size_t i = 0; i < b.blocks().size(); ++i) {
    using base::stringify;
    os << "\n block #" << i << " (" << stringify(b.blocks()[i]) << ")\n"
       << *b.blocks()[i];
  }
  return os;
}

}  // namespace ir::internal
