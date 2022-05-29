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

namespace module {
struct Module;
}  // namespace module
namespace ir {
struct BasicBlock;
struct GenericFn;
struct Scope;
struct ScopeContext;
struct Subroutine;
struct UnboundScope;

template <typename T>
concept MemcpySerializable =
    std::is_arithmetic_v<T> or
    base::one_of<T, bool, std::byte, Block, ModuleId, Reg, Slice>;

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

  void write(MemcpySerializable auto const &x) {
    write_bytes(ByteRange(std::addressof(x)));
  }

  // The only address that makes sense across serialization boundaries is the
  // null address, so we can simply skip serialization.
  void write(addr_t a) { ASSERT(a == Null()); }


  void write(std::string_view s) {
    base::Serialize(*this, s.size());
    write_bytes(absl::MakeConstSpan(
        reinterpret_cast<std::byte const *>(s.data()), s.size()));
  }

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

  void write(Scope const &) { NOT_YET(); }
  void write(ScopeContext const &) { NOT_YET(); }
  void write(UnboundScope const &) { NOT_YET(); }
  void write(Subroutine const *) { NOT_YET(); }
  void write(GenericFn const &) { NOT_YET(); }
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

struct InstructionDeserializer {
  explicit InstructionDeserializer(std::string const *content)
      : content_(*ASSERT_NOT_NULL(content)) {}

  absl::Span<std::byte const> read_bytes(size_t num_bytes) {
    auto span = absl::MakeConstSpan(
        reinterpret_cast<std::byte const *>(content_.data()), num_bytes);
    content_.remove_prefix(num_bytes);
    return span;
  }

  bool read(MemcpySerializable auto &x) {
    std::memcpy(&x, content_.data(), sizeof(x));
    content_.remove_prefix(sizeof(x));
    return true;
  }

  bool read(Scope &) { NOT_YET(); }
  bool read(ScopeContext &) { NOT_YET(); }
  bool read(UnboundScope &) { NOT_YET(); }
  bool read(Subroutine const *&) { NOT_YET(); }
  bool read(GenericFn &) { NOT_YET(); }
  bool read(type::Argument &) { NOT_YET(); }
  bool read(BasicBlock const *&) { NOT_YET(); }
  bool read(type::Enum *&) { NOT_YET(); }
  bool read(type::Flags *&) { NOT_YET(); }
  bool read(type::Struct const *&) { NOT_YET(); }
  bool read(type::Pointer const *&) { NOT_YET(); }

  bool read(type::Type &t) {
    size_t index;
    if (not base::Deserialize(*this, index)) { return false; }
    t = type_system_.from_index(index);
    return true;
  }

  bool read(addr_t &a) {
    a = Null();
    return true;
  }

  bool read(std::string_view &s) {
    size_t length;
    if (not base::Deserialize(*this, length)) { return false; }
    s = std::string_view(content_.data(), length);
    content_.remove_prefix(length);
    return true;
  }

  template <typename T>
  bool read(RegOr<T> &r) {
    bool is_reg;
    if (not base::Deserialize(*this, is_reg)) { return false; }
    if (is_reg) {
      Reg reg;
      if (not base::Deserialize(*this, reg)) { return false; }
      r = reg;
    } else {
      T value;
      if (not base::Deserialize(*this, value)) { return false; }
      r = std::move(value);
    }
    return true;
  }
  template <typename T>
  bool read(std::optional<T> &t) {
    bool is_engaged;
    if (not base::Deserialize(*this, is_engaged)) { return false; }
    if (is_engaged) {
      T value;
      if (not base::Deserialize(*this, value)) { return false; }
      t = std::move(value);
    } else {
      t = std::nullopt;
    }
    return true;
  }

 private:
  std::string_view content_;
  type::TypeSystem const &type_system_ = type::GlobalTypeSystem;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_SERIALIZER_H
