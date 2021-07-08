#ifndef ICARUS_IR_INTERPRETER_BYTE_CODE_WRITER_H
#define ICARUS_IR_INTERPRETER_BYTE_CODE_WRITER_H

#include <cstddef>
#include <cstdint>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "base/debug.h"
#include "base/serialize.h"
#include "base/untyped_buffer.h"

namespace ir {
struct BasicBlock;
}  // namespace ir

namespace interpreter {
struct ByteCodeWriter {
  explicit ByteCodeWriter(base::untyped_buffer* buffer)
      : buffer_(*ASSERT_NOT_NULL(buffer)) {}
  ~ByteCodeWriter() { ASSERT(replacements_.size() == 0u); }

  void write_bytes(absl::Span<std::byte const> bytes) {
    buffer_.write(buffer_.size(), bytes.data(), bytes.size());
  }

  void write(ir::BasicBlock const* block) {
    replacements_[block].push_back(buffer_.size());
    buffer_.append_bytes(sizeof(ir::BasicBlock*));
  }

  void set_block(ir::BasicBlock const* b) {
    offsets_.emplace(b, buffer_.size());
  }

  void Finalize() {
    for (auto const& [block, locs] : replacements_) {
      auto iter = offsets_.find(block);
      ASSERT(iter != offsets_.end());
      for (size_t loc : locs) { buffer_.set(loc, iter->second); }
    }
    replacements_.clear();
  }

 private:
  base::untyped_buffer& buffer_;
  absl::flat_hash_map<ir::BasicBlock const*, uintptr_t> offsets_;
  absl::flat_hash_map<ir::BasicBlock const*, std::vector<size_t>> replacements_;
};

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_BYTE_CODE_WRITER_H
