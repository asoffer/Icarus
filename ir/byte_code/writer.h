#ifndef ICARUS_IR_BYTE_CODE_WRITER_H
#define ICARUS_IR_BYTE_CODE_WRITER_H

#include <cstddef>
#include <cstdint>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "base/debug.h"
#include "ir/byte_code/byte_code.h"

namespace ir {
struct BasicBlock;

struct ByteCodeWriter {
  explicit ByteCodeWriter(ByteCode* byte_code)
      : byte_code_(*ASSERT_NOT_NULL(byte_code)) {}
  ~ByteCodeWriter() { ASSERT(replacements_.size() == 0u); }

  void write_bytes(absl::Span<std::byte const> bytes) {
    byte_code_.write_bytes(bytes);
  }

  void write(BasicBlock const* block) {
    replacements_[block].push_back(byte_code_.append_block_slot());
  }

  void set_block(BasicBlock const* b) {
    offsets_.emplace(b, byte_code_.size());
  }

  void Finalize() && {
    for (auto const& [block, locs] : replacements_) {
      auto iter = offsets_.find(block);
      ASSERT(iter != offsets_.end());
      for (size_t loc : locs) { byte_code_.set(loc, iter->second); }
    }
    replacements_.clear();
  }

 private:
  ByteCode& byte_code_;
  absl::flat_hash_map<BasicBlock const*, uintptr_t> offsets_;
  absl::flat_hash_map<BasicBlock const*, std::vector<size_t>> replacements_;
};

}  // namespace ir

#endif  // ICARUS_IR_BYTE_CODE_WRITER_H
