#include "precompiled/module.h"

#include "base/file.h"
#include "core/parameters.h"
#include "precompiled/serialize.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic_function.h"
#include "type/legacy_generic.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/type.h"

namespace precompiled {

PrecompiledModule::PrecompiledModule(std::string identifier, ir::ModuleId,
                                     module::SharedContext& shared_context)
    : module::Module(std::move(identifier)), shared_context_(shared_context) {}

absl::StatusOr<std::pair<ir::ModuleId, PrecompiledModule*>>
PrecompiledModule::Make(std::string_view label, ModuleProto const& module_proto,
                        module::ModuleMap const& module_map,
                        module::SharedContext& context) {
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
    absl::Status status = Load(import_name, module_map, context).status();
    if (not status.ok()) { return status; }
  }

  std::string identifier = module_proto.identifier();

  return context.module_table().add_module<PrecompiledModule>(
      std::move(identifier), context);
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
  auto* entry = module_map.by_import_name(import_name);
  if (not entry) {
    return absl::NotFoundError(absl::StrFormat(
        R"(Failed to find module map entry for '%s')", import_name));
  }

  auto [label, icm, unused_import_name] = *entry;
  auto maybe_content                    = base::ReadFileToString(icm);
  if (not maybe_content) {
    return absl::InvalidArgumentError(absl::StrFormat(
        "Failed to load precompiled module `%s`.", import_name));
  }

  ModuleProto module_proto;

  if (not module_proto.ParseFromString(*maybe_content)) {
    return absl::InvalidArgumentError(
        "Failed to deserialize module identifier.");
  }

  auto result = Make(label, module_proto, module_map, shared_context);
  if (not result.ok()) { return std::move(result).status(); }
  auto [id, pm] = *result;

  type::TypeSystem local_type_system;
  FromProto(module_proto.type_system(), local_type_system);

  for (auto const& [symbol, infos] : module_proto.symbols()) {
    auto& s = pm->symbols_[symbol];
    for (auto const& info : infos.symbol()) {
      s.push_back({
          .qualified_type = type::QualType(
              local_type_system.from_index(info.value().type_id()),
              type::Qualifiers::FromValue(info.qualifiers())),
          .value      = DeserializeValue(shared_context.module_table(),
                                    module_proto.modules(), local_type_system,
                                    info.value()),
          .visibility = info.visible() ? module::Module::Visibility::Exported
                                       : module::Module::Visibility::Private,
      });
    }
  }

  pm->subroutines_.reserve(module_proto.subroutines().size());
  for (auto const& subroutine_proto : module_proto.subroutines()) {
    auto& subroutine = pm->subroutines_.emplace_back();
    bool success     = ir::Subroutine::FromProto(
        subroutine_proto, shared_context.instruction_deserializer(),
        subroutine);
    if (not success) {
      return absl::InvalidArgumentError("Failed to deserialized subroutine.");
    }
  }

  pm->proto_ = std::move(module_proto);

  return std::pair<ir::ModuleId, PrecompiledModule const*>(id, pm);
}

module::Module::FunctionInformation PrecompiledModule::Function(
    ir::LocalFnId id) const {
  ASSERT(id.value() < proto_.subroutines().size());
  auto const& subroutine = subroutines_[id.value()];
  return module::Module::FunctionInformation{
      .type       = &subroutine.type()->as<type::Function>(),
      .subroutine = &subroutine,
  };
}

}  // namespace precompiled
