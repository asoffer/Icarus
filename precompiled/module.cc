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

PrecompiledModule::PrecompiledModule(std::string identifier, ir::ModuleId,
                                     ModuleProto proto,
                                     module::SharedContext& shared_context)
    : module::Module(std::move(identifier)), shared_context_(shared_context) {
  type::TypeSystem local_type_system;
  FromProto(proto.type_system(), local_type_system);

  for (auto const& [symbol, infos] : proto.symbols()) {
    auto& s = symbols_[symbol];
    for (auto const& info : infos.symbol()) {
      s.push_back({
          .qualified_type = type::QualType(
              local_type_system.from_index(info.value().type_id()),
              type::Quals::FromValue(info.qualifiers())),
          .value =
              DeserializeValue(shared_context.module_table(), proto.modules(),
                               local_type_system, info.value()),
          .visibility = info.visible() ? module::Module::Visibility::Exported
                                       : module::Module::Visibility::Private,
      });
    }
  }
}

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
      std::move(identifier), std::move(module_proto), context);

  return std::pair<ir::ModuleId, PrecompiledModule const*>(id, m);
}

absl::Span<module::Module::SymbolInformation const> PrecompiledModule::Symbols(
    std::string_view name) const {
  auto iter           = symbols_.find(name);
  if (iter == symbols_.end()) { return {}; }
  return iter->second;
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
