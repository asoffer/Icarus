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
PrecompiledModule::Make(
    std::string_view label,
    std::string const& file_content,
    absl::flat_hash_map<std::string, std::pair<std::string, std::string>> const&
        module_map,
    module::SharedContext& context) {
  ModuleProto module_proto;

  if (not module_proto.ParseFromString(file_content)) {
    return absl::InvalidArgumentError(
        "Failed to deserialize module identifier.");
  }

  for (auto const& [id, name] : module_proto.modules()) {
    ir::ModuleId mod_id(id);
    ASSERT(mod_id != ir::ModuleId::Invalid());
    if (mod_id == ir::ModuleId::Builtin() or
        mod_id == ir::ModuleId::Foreign() or name == label) {
      continue;
    }

    // If we have already loaded this module, there's no need to continue.
    if (context.module_table().module(name).second) { continue; }

    std::string_view module_label;
    std::string_view icm;
    for (auto const& [file, label_and_icm] : module_map) {
      if (label_and_icm.first == name ) {
        module_label = label_and_icm.first;
        icm = label_and_icm.second;
        break;
      }
    }
    ASSERT(icm.empty() == false);
    if (auto maybe_content = base::ReadFileToString(std::string(icm))) {
      auto maybe_module =
          Make(module_label, *maybe_content, module_map, context);
      if (not maybe_module.ok()) { return std::move(maybe_module).status(); }
    }
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

absl::StatusOr<std::pair<ir::ModuleId, PrecompiledModule const*>>
PrecompiledModule::Load(
    std::string const& file_name, absl::Span<std::string const> lookup_paths,
    absl::flat_hash_map<std::string, std::pair<std::string, std::string>> const&
        module_map,
    module::SharedContext& shared_context) {
  if (!file_name.starts_with("/")) {
    for (std::string_view base_path : lookup_paths) {
      auto iter = module_map.find(absl::StrCat(base_path, "/", file_name));
      if (iter == module_map.end()) {
        continue;
      }
      auto const& [label, icm] = iter->second;
      if (auto maybe_content = base::ReadFileToString(icm)) {
        return PrecompiledModule::Make(label, *maybe_content, module_map,
                                       shared_context);
      }
    }
  }

  auto iter = module_map.find(file_name);
  if (iter == module_map.end()) {
    return absl::NotFoundError(absl::StrFormat(
        R"(Failed to find module map entry for '%s')", file_name));
  }

  auto const& [label, icm] = iter->second;
  if (auto maybe_content = base::ReadFileToString(icm)) {
    return precompiled::PrecompiledModule::Make(label, *maybe_content,
                                                module_map, shared_context);
  }

  return absl::NotFoundError(absl::StrFormat(
      R"(Failed to load precompiled module for '%s')", file_name));
}

}  // namespace precompiled
