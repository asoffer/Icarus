#ifndef ICARUS_MODULE_READER_H
#define ICARUS_MODULE_READER_H

#include "base/debug.h"
#include "base/meta.h"
#include "base/serialize.h"
#include "type/primitive.h"
#include "type/serialize.h"

namespace module {

struct ModuleReader {
  explicit ModuleReader(std::string_view s)
      : head_(reinterpret_cast<std::byte const*>(s.begin())),
        end_(reinterpret_cast<std::byte const*>(s.end())) {}

  absl::Span<std::byte const> read_bytes(size_t num_bytes) {
    ASSERT(head_ + num_bytes < end_);
    std::byte const* p = head_;
    head_ += num_bytes;
    return absl::MakeConstSpan(p, num_bytes);
  }

  template <typename T>
  bool read(T& t) requires(std::is_enum_v<T> or std::is_arithmetic_v<T> or
                           base::meta<T> == base::meta<type::Quals>) {
    if (end_ - head_ < sizeof(t)) { return false; }
    std::memcpy(&t, head_, sizeof(t));
    head_ += sizeof(t);
    return true;
  }

  bool read(Module::SymbolInformation& info) {
    if (not base::Deserialize(*this, info.qualified_type)) { return false; }
    ssize_t num_read = type::DeserializeValue(
        info.qualified_type.type(), absl::MakeConstSpan(head_, end_ - head_),
        info.value);
    if (num_read < 0) { return false; }
    head_ += num_read;
    return true;
  }

  bool read(type::QualType& qt) {
    auto quals  = type::Quals::Unqualified();
    if (not base::Deserialize(*this, quals)) { return false; }
    ir::CompleteResultBuffer buffer;
    ssize_t num_read = type::DeserializeValue(
        type::Type_, absl::MakeConstSpan(head_, end_ - head_), buffer);
    if (num_read < 0) { return false; }
    head_ += num_read;
    ASSERT(buffer.num_entries() == 1);
    qt = type::QualType(buffer[0].get<type::Type>(), quals);
    return true;
  }

 private:
  std::byte const* head_;
  std::byte const* end_;
};

}  // namespace module

#endif  // ICARUS_MODULE_READER_H
