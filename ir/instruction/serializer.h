#ifndef ICARUS_IR_INSTRUCTION_SERIALIZER_H
#define ICARUS_IR_INSTRUCTION_SERIALIZER_H

#include <cstddef>
#include <memory>
#include <type_traits>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "base/flyweight_set.h"
#include "base/meta.h"
#include "ir/instruction/instruction.pb.h"
#include "ir/value/block.h"
#include "ir/value/module_id.h"
#include "ir/value/slice.h"
#include "type/argument.h"
#include "type/system.h"

namespace ir {
struct BasicBlock;
struct GenericFn;

struct InstructionSerializer {
  explicit InstructionSerializer(
      absl::flat_hash_map<BasicBlock const *, size_t> const *index_map,
      base::flyweight_set<base::MetaValue> const *instruction_ids)
      : index_map_(*ASSERT_NOT_NULL(index_map)),
        instruction_ids_(*ASSERT_NOT_NULL(instruction_ids)) {}

  void write_bytes(absl::Span<std::byte const> bytes) {
    output_->mutable_content()->append(std::string_view(
        reinterpret_cast<char const *>(bytes.data()), bytes.size()));
  }

  void write(auto n) requires(std::is_arithmetic_v<decltype(n)>) {
    write_bytes(ByteRange(std::addressof(n)));
  }
  void write(bool b) { write_bytes(ByteRange(std::addressof(b))); }
  void write(std::byte b) { write_bytes(ByteRange(std::addressof(b))); }
  void write(Block b) { write_bytes(ByteRange(std::addressof(b))); }
  void write(ModuleId m) { write_bytes(ByteRange(std::addressof(m))); }
  void write(Reg r) { write_bytes(ByteRange(std::addressof(r))); }
  template <typename T>
  void write(RegOr<T> r) {
    if (r.is_reg()) {
      base::Serialize(*this, true, r.reg());
    } else {
      base::Serialize(*this, false, r.value());
    }
  }
  template <typename T>
  void write(std::optional<T> const &t) {
    if (t) {
      base::Serialize(*this, true, *t);
    } else {
      base::Serialize(*this, false);
    }
  }

  void write(Slice s) { write_bytes(ByteRange(std::addressof(s))); }
  void write(GenericFn const & f) { NOT_YET(); }
  void write(type::Argument const &) { NOT_YET(); }

  void write(BasicBlock *b) { write(block(b)); }
  void write(BasicBlock const *b) { write(block(b)); }

  void write(type::Type t) { write(type_system_.index(t)); }

  template <typename T>
  void SetIdentifier() {
    output_->set_identifier(instruction_ids_.index(base::meta<T>));
    ICARUS_DEBUG_ONLY(output_->set_mnemonic(typeid(T).name());)
  }

  size_t block(BasicBlock const *block) const {
    auto iter = index_map_.find(block);
    ASSERT(iter != index_map_.end());
    return iter->second;
  }

  void set_output(InstructionProto &output) { output_ = &output; }

 private:
  template <typename T>
  static absl::Span<std::byte const> ByteRange(T const *p) requires(
      std::is_trivially_copyable_v<T>) {
    auto const *ptr = reinterpret_cast<std::byte const *>(p);
    return absl::MakeConstSpan(ptr, ptr + sizeof(T));
  }

  InstructionProto *output_;
  absl::flat_hash_map<BasicBlock const *, size_t> const &index_map_;
  base::flyweight_set<base::MetaValue> const &instruction_ids_;
  type::TypeSystem const &type_system_ = type::GlobalTypeSystem;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_SERIALIZER_H
