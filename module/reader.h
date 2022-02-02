#ifndef ICARUS_MODULE_READER_H
#define ICARUS_MODULE_READER_H

#include "base/debug.h"

namespace module {

struct ModuleReader {
  explicit ModuleReader(std::string_view s) : head_(s.begin()), end_(s.end()) {}

  absl::Span<std::byte const> read_bytes(size_t num_bytes) {
    ASSERT(head_ + num_bytes < end_);
    std::byte const* p = reinterpret_cast<std::byte const*>(head_);
    head_ += num_bytes;
    return absl::MakeConstSpan(p, num_bytes);
  }

  void read(Module::SymbolInformation& information) {
    base::Deserialize(*this, information.qualified_type, information.value);
  }

  void write(std::string_view s) {
    write_bytes(absl::MakeConstSpan(
        reinterpret_cast<std::byte const*>(s.data()), s.size()));
  }

  void read(type::QualType& qt) {
    type::Quals quals;
    type::Type t;
    base::Deserialize(*this, quals, t);
    qt = type::QualType(qt);
  }

  void read(type::Type& t) {
    type::Primitive::Kind k;
    base::Deserialize(*this, k);
    t = type::MakePrimitive(k);
  }

 private:
  char const* head_;
  char const* end_;
};

}  // namespace module

#endif  // ICARUS_MODULE_READER_H
