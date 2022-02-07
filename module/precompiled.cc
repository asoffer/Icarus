#include "module/precompiled.h"

namespace module {

absl::StatusOr<PrecompiledModule> PrecompiledModule::Make(
    std::string_view file_content,
    base::flyweight_map<std::pair<std::string, type::Function const*>,
                        void (*)()>* foreign_fn_map) {
  ModuleReader r(file_content, foreign_fn_map);
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
