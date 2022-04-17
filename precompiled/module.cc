#include "precompiled/module.h"

#include "base/file.h"
#include "core/parameters.h"
#include "precompiled/serialize.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic.h"
#include "type/generic_function.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/type.h"

namespace precompiled {

absl::StatusOr<std::pair<ir::ModuleId, PrecompiledModule const*>>
PrecompiledModule::Make(std::string const& file_content,
                        module::SharedContext& context) {
  ModuleProto module_proto;

  if (not module_proto.ParseFromString(file_content)) {
    return absl::InvalidArgumentError(
        "Failed to deserialize module identifier.");
  }

  std::string identifier = module_proto.identifier();

  auto [id, m] = context.module_table().add_module<PrecompiledModule>(
      std::move(identifier), std::move(module_proto));

  return std::pair<ir::ModuleId, PrecompiledModule const*>(id, m);
}

absl::Span<module::Module::SymbolInformation const> PrecompiledModule::Symbols(
    std::string_view name) const {
  auto const& symbols = proto_.symbols();
  auto iter           = symbols.find(name);
  if (iter == symbols.end()) { return {}; }
  // TODO:
  // return iter->second;
  return {};
}

absl::StatusOr<std::pair<ir::ModuleId, precompiled::PrecompiledModule const*>>
PrecompiledModule::Load(
    std::string const& file_name, absl::Span<std::string const> lookup_paths,
    absl::flat_hash_map<std::string, std::string> const& module_map,
    module::SharedContext& shared_context) {
  if (!file_name.starts_with("/")) {
    for (std::string_view base_path : lookup_paths) {
      auto iter = module_map.find(absl::StrCat(base_path, "/", file_name));
      if (iter == module_map.end()) { continue; }
      if (auto maybe_content = base::ReadFileToString(iter->second)) {
        return precompiled::PrecompiledModule::Make(*maybe_content,
                                                    shared_context);
      }
    }
  }

  auto iter = module_map.find(file_name);
  if (iter == module_map.end()) {
    return absl::NotFoundError(absl::StrFormat(
        R"(Failed to find module map entry for '%s')", file_name));
  }

  if (auto maybe_content = base::ReadFileToString(iter->second)) {
    return precompiled::PrecompiledModule::Make(*maybe_content, shared_context);
  }

  return absl::NotFoundError(absl::StrFormat(
      R"(Failed to load precompiled module for '%s')", file_name));
}

}  // namespace precompiled
