#include "module/precompiled.h"

#include "type/serialize.h"
#include "type/system.h"

namespace module {

absl::StatusOr<PrecompiledModule> PrecompiledModule::Make(
    std::string_view file_content, SharedContext& context) {
  PrecompiledModule m;

  type::TypeSystem local_system;
  if (not type::DeserializeTypeSystem(file_content, local_system)) {
    return absl::InvalidArgumentError("Failed to deserialize type system.");
  }

  ModuleReader r(file_content, context, local_system);
  if (not base::Deserialize(r, m.identifier_, m.symbols_)) {
    return absl::InvalidArgumentError("Failed to deserialize symbol table.");
  }
  return m;
}

absl::Span<Module::SymbolInformation const> PrecompiledModule::Exported(
    std::string_view name) {
  auto iter = symbols_.find(name);
  if (iter == symbols_.end()) { return {}; }
  return iter->second;
}

}  // namespace module
