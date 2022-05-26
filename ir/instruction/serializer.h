#ifndef ICARUS_IR_INSTRUCTION_SERIALIZER_H
#define ICARUS_IR_INSTRUCTION_SERIALIZER_H

#include <cstddef>
#include <memory>
#include <type_traits>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "base/meta.h"
#include "base/flyweight_set.h"
#include "ir/instruction/instruction.pb.h"

namespace ir {
struct BasicBlock;

struct InstructionSerializer {
  explicit InstructionSerializer(
      absl::flat_hash_map<BasicBlock const *, size_t> const *index_map,
      base::flyweight_set<base::MetaValue> *type_ids)
      : index_map_(*ASSERT_NOT_NULL(index_map)),
        type_ids_(*ASSERT_NOT_NULL(type_ids)) {}

  void write_bytes(absl::Span<std::byte const> bytes) {
    output_->mutable_content()->append(std::string_view(
        reinterpret_cast<char const *>(bytes.data()), bytes.size()));
  }

  template <typename T>
  void write(T const &t) requires(std::is_trivially_copyable_v<T>) {
    auto const *p = reinterpret_cast<std::byte const *>(std::addressof(t));
    write_bytes(absl::MakeConstSpan(p, p + sizeof(T)));
  }

  void write(BasicBlock *b) { write(block(b)); }
  void write(BasicBlock const *b) { write(block(b)); }

  template <typename T>
  void SetIdentifier() {
    // TODO: Inserting is fine, but we need to create a global list of all of
    // these anyway for decoding, so it doesn't really save us anything.
    auto [iter, inserted] = type_ids_.insert(base::meta<T>);
    output_->set_identifier(type_ids_.index(iter));
  }

  size_t block(BasicBlock const * block) const {
    auto iter = index_map_.find(block);
    ASSERT(iter != index_map_.end());
    return iter->second;
  }

  void set_output(InstructionProto &output) { output_ = &output; }

 private:
  InstructionProto *output_;
  absl::flat_hash_map<BasicBlock const *, size_t> const &index_map_;
  base::flyweight_set<base::MetaValue> &type_ids_;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_SERIALIZER_H
