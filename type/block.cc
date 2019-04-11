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

core::Bytes Block::bytes(core::Arch const &) const {
  return core::Host().ptr_bytes;
}

core::Alignment Block::alignment(core::Arch const &) const {
  return core::Host().ptr_alignment;
}

bool Block::ReinterpretAs(Type const *t) const { return t == this; }

Cmp Block::Comparator() const { return Cmp::None; }
}  // namespace type
