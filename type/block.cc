#include "type/block.h"

namespace type {
static type::Block blk_;
Block *Blk() { return &blk_; }

void Block::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {}

void Block::EmitCopyAssign(Type const *from_type, ir::Results const &from,
                           ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  UNREACHABLE();
}

void Block::EmitMoveAssign(Type const *from_type, ir::Results const &from,
                          ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  UNREACHABLE();
}

void Block::EmitInit(ir::Register id_reg, Context *ctx) const {
  UNREACHABLE();
}

void Block::EmitRepr(ir::Results const &, Context *ctx) const { UNREACHABLE(); }

void Block::WriteTo(std::string *result) const { result->append("block"); }

ir::Results Block::PrepareArgument(Type const *from, ir::Results const &val,
                                      Context *ctx) const {
  UNREACHABLE();
}

layout::Bytes Block::bytes(layout::Arch const &) const {
  return layout::Host().ptr_bytes;
}

layout::Alignment Block::alignment(layout::Arch const &) const {
  return layout::Host().ptr_alignment;
}

Cmp Block::Comparator() const { return Cmp::None; }
}  // namespace type
