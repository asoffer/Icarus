#include "module/importer.h"

#include <string>
#include <string_view>

namespace module {

bool Importer::SetImplicitlyEmbeddedModules(
    absl::Span<std::string const> module_locators) {
  ASSERT(embedded_module_ids_.size() == 0u);
  embedded_module_ids_.reserve(module_locators.size());
  for (std::string_view module : module_locators) {
    auto id = Import(module);
    embedded_module_ids_.push_back(id);
    if (id == ir::ModuleId::Invalid()) { return false; }
  }

  return true;
}

}  // namespace module
