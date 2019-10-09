#include "ir/compiled_fn.h"

#include "ast/ast.h"
#include "core/arch.h"
#include "type/function.h"
#include "type/pointer.h"

namespace ir {

CompiledFn::CompiledFn(
    type::Function const *fn_type,
    core::FnParams<type::Typed<ast::Expression const *>> params)
    : type_(fn_type),
      params_(std::move(params)),
      num_regs_(
          static_cast<int32_t>(type_->input.size() + type_->output.size())) {
  auto arch = core::Interpretter();
  int32_t i = 0;
  for (auto *t : type_->input) {
    auto entry = core::Bytes{0};
    if (t->is_big()) {
      entry = core::FwdAlign(reg_size_, core::Alignment::Get<Addr>());
    } else {
      entry = core::FwdAlign(reg_size_, t->alignment(arch));
    }
    compiler_reg_to_offset_.emplace(ir::Reg::Arg(i++), entry.value());
    reg_size_ =
        entry + (t->is_big() ? core::Bytes::Get<Addr>() : t->bytes(arch));
  }

  ASSERT(params_.size() ==
         fn_type->input.size());  // TODO is this still true with variadics?
  blocks_.push_back(std::make_unique<BasicBlock>(this));
}

Reg CompiledFn::Reserve(core::Bytes b, core::Alignment a) {
  auto offset = core::FwdAlign(reg_size_, a);
  reg_size_   = offset + b;
  DEBUG_LOG("reserve")
  ("New size = ", reg_size_, ", because type's size is ", b);

  // TODO starts at `n`, where `n` is the number of function arguments.
  Reg r{compiler_reg_to_offset_.size()};
  compiler_reg_to_offset_.emplace(r, offset.value());
  ++num_regs_;
  return r;
}

Reg CompiledFn::Reserve(type::Type const *t) {
  auto arch = core::Interpretter();
  return Reserve(t->bytes(arch), t->alignment(arch));
}

Reg CompiledFn::Alloca(type::Type const *t) {
  Reg r = Reserve(type::Ptr(t));
  allocs_.allocate(t, r);
  return r;
}

std::ostream &operator<<(std::ostream &os, ir::CompiledFn const &f) {
  os << "\n" << f.name() << ": " << f.type_->to_string();
  for (size_t i = 0; i < f.blocks_.size(); ++i) {
    os << "\n block #" << i << "\n" << *f.blocks_[i];
  }
  return os;
}

std::string CompiledFn::name() const {
  std::stringstream ss;
  ss << this;
  return ss.str();
}

}  // namespace ir
