#ifndef ICARUS_MODULE_READER_H
#define ICARUS_MODULE_READER_H

#include "base/debug.h"
#include "base/meta.h"
#include "base/serialize.h"
#include "type/primitive.h"

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

  bool read(Module::SymbolInformation& information) {
    return base::Deserialize(*this, information.qualified_type, information.value);
  }

  bool read(type::QualType& qt) {
    auto quals = type::Quals::Unqualified();
    type::Type t;
    bool result = base::Deserialize(*this, quals, t);
    qt = type::QualType(qt);
    return result;
  }

  bool read(type::Type& t) {
    type::Primitive::Kind k;
    bool result = base::Deserialize(*this, k);
    t = type::MakePrimitive(k);
    return result;
  }

 private:
  std::byte const* head_;
  std::byte const* end_;
};

}  // namespace module

#endif  // ICARUS_MODULE_READER_H
