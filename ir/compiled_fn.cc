#include "ir/compiled_fn.h"

#include "ast/ast.h"
#include "core/arch.h"
#include "ir/arguments.h"
#include "type/function.h"
#include "type/pointer.h"

namespace ir {
thread_local CompiledFn *CompiledFn::Current{nullptr};

CompiledFn::CompiledFn(
    Module *mod, type::Function const *fn_type,
    core::FnParams<type::Typed<ast::Expression const *>> params)
    : type_(fn_type),
      params_(std::move(params)),
      num_regs_(
          static_cast<int32_t>(type_->input.size() + type_->output.size())),
      mod_(mod) {
  // Set the references for arguments and returns
  for (int32_t i = -static_cast<int32_t>(type_->output.size());
       i < static_cast<int32_t>(type_->input.size()); ++i) {
    references_[Reg(i)];
  }

  auto arch = core::Interpretter();
  int32_t i = 0;
  for (auto *t : type_->input) {
    auto entry = core::Bytes{0};
    if (t->is_big()) {
      entry = core::FwdAlign(reg_size_, core::Alignment{alignof(Addr)});
    } else {
      entry = core::FwdAlign(reg_size_, t->alignment(arch));
    }
    compiler_reg_to_offset_.emplace(ir::Reg::Arg(i++), entry.value());
    reg_size_ =
        entry + (t->is_big() ? core::Bytes{sizeof(Addr)} : t->bytes(arch));
  }

  ASSERT(params_.size() ==
         fn_type->input.size());  // TODO is this still true with variadics?
  blocks_.emplace_back(this);
}

Cmd const *CompiledFn::Command(Reg reg) const {
  auto iter = reg_to_cmd_.find(reg);
  if (iter == reg_to_cmd_.end()) { return nullptr; }
  return &Command(iter->second);
}

std::ostream &operator<<(std::ostream &os, ir::CompiledFn const &f) {
  os << "\n" << f.name() << ": " << f.type_->to_string();
  for (size_t i = 0; i < f.blocks_.size(); ++i) {
    os << "\n block #" << i << "\n" << f.blocks_[i];
  }
  return os;
}

std::string CompiledFn::name() const {
  std::stringstream ss;
  ss << this;
  return ss.str();
}

}  // namespace ir
