#ifndef ICARUS_IR_BYTE_CODE_WRITER_H
#define ICARUS_IR_BYTE_CODE_WRITER_H

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "base/meta.h"
#include "base/untyped_buffer.h"

namespace ir {
struct BasicBlock;

struct ByteCodeWriter {
  explicit ByteCodeWriter(base::untyped_buffer* buf) : buf_(buf) {}
  ~ByteCodeWriter() { ASSERT(replacements_.size() == 0u); }

  template <typename T,
            std::enable_if_t<base::meta<T> != base::meta<BasicBlock*> and
                                 base::meta<T> != base::meta<BasicBlock const*>,
                             int> = 0>
  void Write(T val) {
    buf_->append(val);
  }

  void Write(BasicBlock const* block) {
    replacements_[block].push_back(buf_->size());
    buf_->append_bytes(sizeof(BasicBlock*));
  }

  void StartBlock(BasicBlock* b) { offsets_.emplace(b, buf_->size()); }

  void MakeReplacements() {
    for (auto const& [block, locs] : replacements_) {
      auto iter = offsets_.find(block);
      ASSERT(iter != offsets_.end());
      for (size_t loc : locs) { buf_->set(loc, iter->second); }
    }
    replacements_.clear();
  }

  // TODO make this private
  base::untyped_buffer* buf_;

  absl::flat_hash_map<BasicBlock const*, uintptr_t> offsets_;
  absl::flat_hash_map<BasicBlock const*, std::vector<size_t>> replacements_;
};

}  // namespace ir

#endif  // ICARUS_IR_BYTE_CODE_WRITER_H
