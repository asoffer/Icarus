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
  static absl::StatusOr<PrecompiledModule> Make(std::string_view file_name,
                                                SharedContext& context);

  absl::Span<SymbolInformation const> Exported(std::string_view name) override;

 private:
  friend bool BaseDeserialize(ModuleReader& r, PrecompiledModule& m) {
    return base::Deserialize(r, m.symbols_);
  }

  PrecompiledModule() = default;

  absl::flat_hash_map<std::string, std::vector<SymbolInformation>> symbols_;
};

}  // namespace module

#endif  // ICARUS_MODULE_PRECOMPILED_H
