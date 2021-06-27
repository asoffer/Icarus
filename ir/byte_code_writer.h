#ifndef ICARUS_IR_BYTE_CODE_WRITER_H
#define ICARUS_IR_BYTE_CODE_WRITER_H

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "base/meta.h"
#include "base/untyped_buffer.h"

namespace ir {
struct BasicBlock;

namespace internal {

template <typename T>
void ReadInto(T& ref, base::untyped_buffer::const_iterator* iter) {
  if constexpr (base::meta<T>.template is_a<absl::flat_hash_map>()) {
    ASSERT(ref.size() == 0u);
    uint16_t num_entries = iter->read<uint16_t>();
    ref.reserve(num_entries);
    for (uint16_t i = 0; i < num_entries; ++i) {
      std::pair<typename T::key_type, typename T::mapped_type> entry;
      ReadInto(entry.first, iter);
      ReadInto(entry.second, iter);
      ref.insert(std::move(entry));
    }
  } else if constexpr (base::meta<T>.template is_a<std::pair>()) {
    ReadInto(ref.first, iter);
    ReadInto(ref.second, iter);

  } else if constexpr (base::meta<T>.template is_a<std::vector>()) {
    ASSERT(ref.size() == 0u);
    uint16_t num_entries = iter->read<uint16_t>();
    ref.reserve(num_entries);
    for (uint16_t i = 0; i < num_entries; ++i) {
      ReadInto(ref.emplace_back(), iter);
    }
  } else if constexpr (base::meta<T> == base::meta<std::string>) {
    ASSERT(ref.size() == 0u);
    uint16_t num_chars = iter->read<uint16_t>();
    ref                = iter->read_bytes_as_string(num_chars);
  } else {
    ref = iter->read<T>();
  }
}
}  // namespace internal

struct ByteCodeWriter {
  explicit ByteCodeWriter(base::untyped_buffer* buf) : buf_(buf) {}
  ~ByteCodeWriter() { ASSERT(replacements_.size() == 0u); }

  template <typename T,
            std::enable_if_t<base::meta<T> != base::meta<BasicBlock*> and
                                 base::meta<T> != base::meta<BasicBlock const*>,
                             int> = 0>
  void Write(T const& val) {
    if constexpr (base::meta<T>.template is_a<std::vector>()) {
      buf_->append<uint16_t>(val.size());
      for (auto const& element : val) { Write(element); }
    } else if constexpr (base::meta<T>.template is_a<std::pair>()) {
      Write(val.first);
      Write(val.second);
    } else if constexpr (base::meta<T> == base::meta<std::string>) {
      buf_->append<uint16_t>(val.size());
      buf_->write(buf_->size(), reinterpret_cast<std::byte const*>(val.data()),
                  val.size());

    } else if constexpr (base::meta<T>.template is_a<absl::flat_hash_map>()) {
      buf_->append<uint16_t>(val.size());
      for (auto const& [k, v] : val) {
        Write(k);
        Write(v);
      }
    } else {
      buf_->append(val);
    }
  }

  void Write(BasicBlock const* block) {
    replacements_[block].push_back(buf_->size());
    buf_->append_bytes(sizeof(BasicBlock*));
  }

  void StartBlock(BasicBlock const* b) { offsets_.emplace(b, buf_->size()); }

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

template <typename T>
struct WriteByteCodeExtension {
  void WriteByteCode(ByteCodeWriter* writer) const {
    std::apply([&](auto const&... field) { (writer->Write(field), ...); },
               static_cast<T const*>(this)->field_refs());
  }
};

template <typename T>
struct ReadFromByteCodeExtension {
  static T ReadFromByteCode(base::untyped_buffer::const_iterator* iter) {
    T result{};
    std::apply([&](auto&... refs) { (internal::ReadInto(refs, iter), ...); },
               result.field_refs());
    return result;
  }
};

template <typename T>
struct ByteCodeExtension {
  using dependencies =
      base::type_list<ReadFromByteCodeExtension<T>, WriteByteCodeExtension<T>>;
};

}  // namespace ir

#endif  // ICARUS_IR_BYTE_CODE_WRITER_H
