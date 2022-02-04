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

  template <typename T>
  void write(T const& t) requires(std::is_enum_v<T> or
                                  std::is_arithmetic_v<T> or
                                  base::meta<T> == base::meta<type::Quals>) {
    auto const* p = reinterpret_cast<std::byte const*>(&t);
    write_bytes(absl::MakeConstSpan(p, p + sizeof(T)));
  }

  void write(Module::SymbolInformation const& information) {
    base::Serialize(*this, information.qualified_type);
    if (information.qualified_type.type() == type::Type_) {
      if (auto const* p =
              information.value[0].get<type::Type>().if_as<type::Primitive>()) {
        base::Serialize(*this, p->kind());
      } else {
        NOT_YET();
      }
    } else {
      NOT_YET();
    }
  }

  void write(std::string_view s) {
    base::Serialize(*this, s.size());
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
