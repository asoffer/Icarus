#include "ir/block_group.h"

#include "core/arch.h"
#include "type/pointer.h"

namespace ir::internal {

BlockGroup::BlockGroup(
    core::FnParams<type::Typed<ast::Declaration const *>> params)
    : params_(std::move(params)) {
  int32_t arg_index = 0;
  auto arch         = core::Interpretter();
  for (const auto &param : params_) {
    auto *t = param.value.type();
    auto entry =
        core::FwdAlign(reg_size_, t->is_big() ? core::Alignment::Get<Addr>()
                                              : t->alignment(arch));
    reg_to_offset_.emplace(ir::Reg::Arg(arg_index++), entry.value());
    reg_size_ =
        entry + (t->is_big() ? core::Bytes::Get<Addr>() : t->bytes(arch));
  }
}

Reg BlockGroup::Reserve(type::Type const *t) {
  auto arch = core::Interpretter();
  return Reserve(t->bytes(arch), t->alignment(arch));
}

Reg BlockGroup::Reserve(core::Bytes b, core::Alignment a) {
  Reg r(reg_to_offset_.size());
  Reserve(r, b, a);
  return r;
}

void BlockGroup::Reserve(Reg r, core::Bytes b, core::Alignment a) {
  // TODO starts at `n`, where `n` is the number of arguments.
  auto offset = core::FwdAlign(reg_size_, a);
  reg_size_   = offset + b;
  DEBUG_LOG("reserve")
  ("New size = ", reg_size_, ", because type's size is ", b);

  reg_to_offset_.emplace(r, offset);
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
