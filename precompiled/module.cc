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
PrecompiledModule::Make(std::string_view label, std::string const& file_content,
                        module::ModuleMap const& module_map,
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

    auto [module_label, icm, import_name] =
        *ASSERT_NOT_NULL(module_map.by_label(name));
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
  auto iter = symbols_.find(name);
  if (iter == symbols_.end()) { return {}; }
  return iter->second;
}

absl::StatusOr<std::pair<ir::ModuleId, PrecompiledModule const*>>
PrecompiledModule::Load(std::string const& import_name,
                        module::ModuleMap const& module_map,
                        module::SharedContext& shared_context) {
  if (auto* entry = module_map.by_import_name(import_name)) {
    auto [label, icm, unused_import_name] = *entry;
    if (auto maybe_content = base::ReadFileToString(icm)) {
      return PrecompiledModule::Make(label, *maybe_content, module_map,
                                     shared_context);
    } else {
      return absl::InvalidArgumentError(absl::StrFormat(
          "Failed to load precompiledmodule `%s`.", import_name));
    }
  } else {
    return absl::NotFoundError(absl::StrFormat(
        R"(Failed to find module map entry for '%s')", import_name));
  }
}

}  // namespace precompiled
