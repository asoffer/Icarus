#include "compiler/constant_binding.h"
#include "ir/results.h"
#include "type/type.h"

namespace compiler {
bool operator==(ConstantBinding const& lhs, ConstantBinding const& rhs) {
  if (lhs.size() != rhs.size()) { return false; }
  for (auto const & [ decl, binding ] : lhs.keys_) {
    if (auto iter = rhs.keys_.find(decl); iter != rhs.keys_.end()) {
      if (binding.type_ != iter->second.type_) { return false; }
      if (not binding.type_->TestEquality(lhs.buf_.raw(binding.offset_),
                                          rhs.buf_.raw(iter->second.offset_))) {
        return false;
      }
    } else {
      return false;
    }
  }
  return true;
}

ir::Results ConstantBinding::get_constant(ast::Declaration const* decl) const {
  auto iter = keys_.find(decl);
  if (iter == keys_.end()) { return ir::Results{}; }
  auto[type, offset] = iter->second;
  return ir::Results::FromRaw(buf_.raw(offset),
                              type->bytes(core::Interpretter()));
}

std::variant<ir::Results, std::pair<size_t, core::Bytes>>
ConstantBinding::reserve_slot(ast::Declaration const* decl,
                              type::Type const* t) {
  auto arch                  = core::Interpretter();
  auto bytes                 = t->bytes(arch);
  auto[iter, newly_inserted] = keys_.try_emplace(decl);
  if (not newly_inserted) {
    return ir::Results::FromRaw(buf_.raw(iter->second.offset_), bytes);
  }
  auto alignment = t->alignment(arch);
  auto offset    = buf_.append_bytes(bytes.value(), alignment.value());
  iter->second   = Binding{t, offset};
  return std::pair(offset, bytes);
}

ir::Results ConstantBinding::set_slot(size_t offset, void const* data,
                                      core::Bytes bytes) {
  std::memcpy(buf_.raw(offset), data, bytes.value());
  return ir::Results::FromRaw(buf_.raw(offset), bytes);
}

}  // namespace compiler
