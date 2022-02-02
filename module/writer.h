#ifndef ICARUS_MODULE_WRITER_H
#define ICARUS_MODULE_WRITER_H

#include <string>
#include <string_view>

#include "absl/types/span.h"
#include "base/debug.h"

namespace module {

struct ModuleWriter {
  explicit ModuleWriter(std::string* out)
      : out_(*ASSERT_NOT_NULL(out)) {}

  void write_bytes(absl::Span<std::byte const> bytes) {
    out_.append(std::string_view(reinterpret_cast<char const*>(bytes.data()),
                                 bytes.size()));
  }

  void write(Module::SymbolInformation const& information) {
    base::Serialize(*this, information.qualified_type, information.value);
  }

  void write(std::string_view s) {
    write_bytes(absl::MakeConstSpan(
        reinterpret_cast<std::byte const*>(s.data()), s.size()));
  }

  void write(type::QualType qt) {
    base::Serialize(*this, qt.quals(), qt.type());
  }

  void write(type::Type t) {
    if (auto const* p = t.if_as<type::Primitive>()) {
      base::Serialize(*this, p->kind());
    } else {
      NOT_YET();
    }
  }

 private:
  std::string& out_;
};

}  // namespace module

#endif  // ICARUS_MODULE_WRITER_H
