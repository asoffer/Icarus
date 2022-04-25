#include "ir/subroutine.h"

#include "type/system.h"

namespace ir {
namespace {

absl::flat_hash_map<BasicBlock const *, size_t> BasicBlockIndexMap(
    Subroutine const &subroutine) {
  absl::flat_hash_map<BasicBlock const *, size_t> index_map;

  index_map.reserve(subroutine.blocks().size());
  size_t i = 0;
  for (auto const *block : subroutine.blocks()) {
    index_map.emplace(block, i++);
  }

  return index_map;
}

}  // namespace

Subroutine::Subroutine(type::Callable const *type)
    : type_(type), alloc_(parameters().size()) {
  AppendBlock(BasicBlock::DebugInfo{.header = "Entry"});
}

Reg Subroutine::Alloca(type::Type t) { return alloc_.StackAllocate(t); }
Reg Subroutine::Alloca(core::TypeContour tc) {
  return alloc_.StackAllocate(tc);
}

std::ostream &operator<<(std::ostream &os, Subroutine const &s) {
  os << "subroutine: " << s.type()->to_string() << "\n" << s.alloc_;
  for (size_t i = 0; i < s.blocks().size(); ++i) {
    os << "\n block #" << i << " (" << s.blocks()[i] << ")\n" << *s.blocks()[i];
  }
  return os;
}

SubroutineProto Subroutine::ToProto() const {
  SubroutineProto result;
  result.set_callable_type_id(type::GlobalTypeSystem.index(type::Type(type_)));

  auto index_map = BasicBlockIndexMap(*this);
  absl::flat_hash_map<base::MetaValue, size_t> type_id_map;
  InstructionSerializer serializer(&index_map, &type_id_map);

  for (auto const *block : blocks()) {
    *result.add_basic_block() = block->ToProto(serializer);
  }

  ICARUS_DEBUG_ONLY(size_t i = 0;)
  for_each_alloc([&](type::Type t, Reg r) {
    ASSERT(r == ir::Reg::StackAllocation(i++));
    result.add_allocation()->set_type_id(type::GlobalTypeSystem.index(t));
  });

  return result;
}

bool Subroutine::FromProto(
    SubroutineProto const &proto,
    absl::FunctionRef<Inst(InstructionProto const &)> deserialize_instruction,
    Subroutine &result) {
  result.type_ = &type::GlobalTypeSystem.from_index(proto.callable_type_id())
                      .as<type::Callable>();
  size_t num_basic_blocks = proto.basic_block().size();
  // Allocate all basic blocks so that `BasicBlock::FromProto` can safely refer
  // to addresses.
  result.blocks_.reserve(num_basic_blocks);
  for (size_t i = 0; i < num_basic_blocks; ++i) {
    result.blocks_.push_back(std::make_unique<BasicBlock>());
  }

  size_t i = 0;
  for (auto const &basic_block : proto.basic_block()) {
    if (not BasicBlock::FromProto(basic_block, result.blocks(),
                                  deserialize_instruction,
                                  *result.blocks_[i++])) {
      return false;
    }
  }

  for (auto const &alloc : proto.allocation()) {
    result.Alloca(type::GlobalTypeSystem.from_index(alloc.type_id()));
  }

  return true;
}

}  // namespace ir
