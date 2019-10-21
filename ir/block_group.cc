#include "ir/block_group.h"

#include "core/arch.h"
#include "type/pointer.h"

namespace ir::internal {

BlockGroup::BlockGroup(
    core::FnParams<type::Typed<ast::Expression const *>> params)
    : params_(std::move(params)) {
  int32_t arg_index = 0;
  auto arch         = core::Interpretter();
  for (const auto &param : params_) {
    auto *t    = param.value.type();
    auto entry = core::Bytes{0};
    if (t->is_big()) {
      entry = core::FwdAlign(reg_size_, core::Alignment::Get<Addr>());
    } else {
      entry = core::FwdAlign(reg_size_, t->alignment(arch));
    }
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
  auto offset = core::FwdAlign(reg_size_, a);
  reg_size_   = offset + b;
  DEBUG_LOG("reserve")
  ("New size = ", reg_size_, ", because type's size is ", b);

  // TODO starts at `n`, where `n` is the number of function arguments.
  Reg r{reg_to_offset_.size()};
  reg_to_offset_.emplace(r, offset.value());
  return r;
}

Reg BlockGroup::Alloca(type::Type const *t) {
  Reg r = Reserve(type::Ptr(t));
  allocs_.allocate(t, r);
  return r;
}

Inliner BlockGroup::inliner() {
  BasicBlock *block = blocks().back();
  AppendBlock();
  return Inliner(reg_to_offset_.size(), blocks().size() - 1, block);
}

}  // namespace ir::internal
