#include "module/precompiled.h"

#include "type/serialize.h"
#include "type/system.h"

namespace module {

absl::StatusOr<std::pair<ir::ModuleId, PrecompiledModule const*>>
PrecompiledModule::Make(std::string const& file_content,
                        SharedContext& context) {
  module_proto::Module module_proto;

  if (not module_proto.ParseFromString(file_content)) {
    return absl::InvalidArgumentError(
        "Failed to deserialize module identifier.");
  }

  std::string identifier = module_proto.identifier();

  auto [id, m] = context.module_table().add_module<PrecompiledModule>(
      std::move(identifier), std::move(module_proto));

  return std::pair<ir::ModuleId, PrecompiledModule const*>(id, m);
}

absl::Span<Module::SymbolInformation const> PrecompiledModule::Symbols(
    std::string_view name) const {
  auto const& symbols = proto_.symbols();
  auto iter           = symbols.find(name);
  if (iter == symbols.end()) { return {}; }
  // TODO:
  // return iter->second;
  return {};
}

}  // namespace module
