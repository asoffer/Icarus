#include "module/precompiled.h"

namespace module {

absl::StatusOr<PrecompiledModule> PrecompiledModule::Make(
    std::string_view file_content) {
  ModuleReader r(file_content);
  PrecompiledModule m;
  base::Deserialize(r, m.symbols_);
  return m;
}

absl::Span<Module::SymbolInformation const> PrecompiledModule::Exported(
    std::string_view name) {
  auto iter = symbols_.find(name);
  if (iter == symbols_.end()) { return {}; }
  return iter->second;
}

}  // namespace module
