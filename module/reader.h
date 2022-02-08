#ifndef ICARUS_MODULE_READER_H
#define ICARUS_MODULE_READER_H

#include "base/meta.h"
#include "base/serialize.h"
#include "module/module.h"
#include "module/shared_context.h"
#include "type/qual_type.h"

namespace module {

struct ModuleReader {
  explicit ModuleReader(std::string_view s, SharedContext& context)
      : head_(reinterpret_cast<std::byte const*>(s.begin())),
        end_(reinterpret_cast<std::byte const*>(s.end())),
        context_(context) {}

  absl::Span<std::byte const> read_bytes(size_t num_bytes);

  template <typename T>
  bool read(T& t) requires(std::is_enum_v<T> or std::is_arithmetic_v<T> or
                           base::meta<T> == base::meta<type::Quals>) {
    if (end_ - head_ < sizeof(t)) { return false; }
    std::memcpy(&t, head_, sizeof(t));
    head_ += sizeof(t);
    return true;
  }

  bool read(std::string& s);
  bool read(Module::SymbolInformation& info);
  bool read(type::QualType& qt);

 private:
  std::byte const* head_;
  std::byte const* end_;

  SharedContext& context_;
};

}  // namespace module

#endif  // ICARUS_MODULE_READER_H
