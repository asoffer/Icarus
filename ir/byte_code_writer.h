#ifndef ICARUS_IR_BYTE_CODE_WRITER_H
#define ICARUS_IR_BYTE_CODE_WRITER_H

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "base/meta.h"
#include "base/untyped_buffer.h"

namespace ir {
struct BasicBlock;

namespace internal_byte_code_writer {

constexpr size_t Log2(size_t n) { return n == 1 ? 0 : 1 + Log2(n / 2); }

// TODO partially copied from instruction/util.h due to dependency issues. Fix this.
template <typename T>
constexpr uint8_t PrimitiveIndex() {
  if constexpr (std::is_integral_v<T> and not std::is_same_v<T, bool>) {
    return Log2(sizeof(T)) * 2 + std::is_signed_v<T>;
  } else if constexpr (std::is_same_v<T, float>) {
    return 0x08;
  } else if constexpr (std::is_same_v<T, double>) {
    return 0x09;
  } else {
    UNREACHABLE(typeid(T).name());
  }
}

}  // namespace internal_byte_code_writer

namespace internal {
template <typename T>
void ReadInto(T& ref, base::untyped_buffer::const_iterator* iter) {
  if constexpr (IsRegOr<T>::value) {
    if (iter->read<bool>()) {
      ref = iter->read<Reg>().get();
    } else {
      ref = iter->read<typename T::type>().get();
    }
  } else {
    ref = iter->read<T>();
  }
}
}  // namespace internal

struct ByteCodeWriter {
  explicit ByteCodeWriter(base::untyped_buffer* buf) : buf_(buf) {}
  ~ByteCodeWriter() { ASSERT(replacements_.size() == 0u); }

  template <
      typename T,
      std::enable_if_t<base::meta<T> != base::meta<BasicBlock*> and
                           base::meta<T> != base::meta<BasicBlock const*> and
                           base::meta<T> != base::meta<ir::Value>,
                       int> = 0>
  void Write(T val) {
    buf_->append(val);
  }

  void Write(ir::Value const& value) {
    if (auto const* b = value.get_if<bool>()) {
      Write(internal_byte_code_writer::PrimitiveIndex<bool>());
      Write(*b);
    } else if (auto const* n = value.get_if<uint8_t>()) {
      Write(internal_byte_code_writer::PrimitiveIndex<uint8_t>());
      Write(*n);
    } else if (auto const* n = value.get_if<uint16_t>()) {
      Write(internal_byte_code_writer::PrimitiveIndex<uint16_t>());
      Write(*n);
    } else if (auto const* n = value.get_if<uint32_t>()) {
      Write(internal_byte_code_writer::PrimitiveIndex<uint32_t>());
      Write(*n);
    } else if (auto const* n = value.get_if<uint64_t>()) {
      Write(internal_byte_code_writer::PrimitiveIndex<uint64_t>());
      Write(*n);
    } else if (auto const* n = value.get_if<int8_t>()) {
      Write(internal_byte_code_writer::PrimitiveIndex<int8_t>());
      Write(*n);
    } else if (auto const* n = value.get_if<int16_t>()) {
      Write(internal_byte_code_writer::PrimitiveIndex<int16_t>());
      Write(*n);
    } else if (auto const* n = value.get_if<int32_t>()) {
      Write(internal_byte_code_writer::PrimitiveIndex<int32_t>());
      Write(*n);
    } else if (auto const* n = value.get_if<int64_t>()) {
      Write(internal_byte_code_writer::PrimitiveIndex<int64_t>());
      Write(*n);
    } else {
      NOT_YET();
    }
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

template <typename T>
struct WriteByteCodeExtension {
  void WriteByteCode(ByteCodeWriter* writer) const {
    writer->Write(T::kIndex);
    auto write = [&](auto const& field) {
      using field_type = std::decay_t<decltype(field)>;
      if constexpr (IsRegOr<field_type>::value) {
        writer->Write(field.is_reg());
        field.apply([&](auto v) { writer->Write(v); });
      } else {
        writer->Write(field);
      }
    };
    std::apply([&](auto const&... field) { (write(field), ...); },
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
