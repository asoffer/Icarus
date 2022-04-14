#ifndef ICARUS_MODULE_PRECOMPILED_H
#define ICARUS_MODULE_PRECOMPILED_H

#include <string>

#include "absl/status/statusor.h"
#include "base/flyweight_map.h"
#include "module/module.h"
#include "module/reader.h"
#include "module/shared_context.h"
#include "type/type.h"

namespace module {

// PrecompiledModule:
//
// Represents a module that has already been compiled, rather than those coming
// from a source file.
struct PrecompiledModule final : Module {
  explicit PrecompiledModule(std::string identifier, ir::ModuleId)
      : Module(std::move(identifier)) {}

  static absl::StatusOr<std::pair<ir::ModuleId, PrecompiledModule const*>> Make(
      std::string_view file_name, SharedContext& context);

  absl::Span<SymbolInformation const> Symbols(std::string_view name) const override;

  FunctionInformation Function(ir::LocalFnId id) const override { NOT_YET(); }

 private:
  friend bool BaseDeserialize(ModuleReader& r, PrecompiledModule& m) {
    std::string id;
    return base::Deserialize(r, id, m.symbols_);
  }

  absl::flat_hash_map<std::string, std::vector<SymbolInformation>> symbols_;

  struct FunctionByteCode {
    std::string_view operator[](size_t n) const {
      return std::string_view(content_.data() + offsets_[n],
                              offsets_[n + 1] - offsets_[n]);
    }

    bool friend BaseDeserialize(ModuleReader & r, FunctionByteCode & f) {
      size_t total_size;
      if (not base::Deserialize(r, total_size, f.offsets_)) { return false; }
      absl::Span<std::byte const > bytes =  r.read_bytes(total_size);
      f.content_ = std::string(reinterpret_cast<char const*>(bytes.data()),
                               bytes.size());
      return true;
    }
   private:
    std::vector<size_t> offsets_;
    std::string content_;
  };
  FunctionByteCode function_byte_code_;
};

}  // namespace module

#endif  // ICARUS_MODULE_PRECOMPILED_H
