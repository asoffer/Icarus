#include "module/precompiled.h"

#include "type/serialize.h"
#include "type/system.h"

namespace module {
namespace {

struct SimpleDeserializer {
  explicit SimpleDeserializer(std::string_view& s) : view_(s) {}

  absl::Span<std::byte const> read_bytes(size_t num_bytes) {
    ASSERT(num_bytes <= view_.size());
    absl::Span<std::byte const> result(
        reinterpret_cast<std::byte const*>(view_.data()), num_bytes);
    view_.remove_prefix(num_bytes);
    return result;
  }

  bool read(std::integral auto& t) {
    if (view_.size() < sizeof(t)) { return false; }
    std::memcpy(&t, view_.data(), sizeof(t));
    view_.remove_prefix(sizeof(t));
    return true;
  }

 private:
  std::string_view &view_;
};

}  // namespace

absl::StatusOr<std::pair<ir::ModuleId, PrecompiledModule const*>>
PrecompiledModule::Make(std::string_view file_content, SharedContext& context) {
  std::string identifier;
  if (SimpleDeserializer d(file_content);
      not base::Deserialize(d, identifier)) {
    return absl::InvalidArgumentError(
        "Failed to deserialize module identifier.");
  }

  auto [id, m] = context.module_table().add_module<PrecompiledModule>(
      std::move(identifier));

  type::TypeSystem local_system;
  if (not type::DeserializeTypeSystem(file_content, context, local_system)) {
    return absl::InvalidArgumentError("Failed to deserialize type system.");
  }

  ModuleReader r(file_content, context, local_system);

  if (not base::Deserialize(r, m->symbols_)) {
    // TODO: Is it necessary to remove the module on failure?
    return absl::InvalidArgumentError("Failed to deserialize symbol table.");
  }
  return std::pair<ir::ModuleId, PrecompiledModule const*>(id, m);
}

absl::Span<Module::SymbolInformation const> PrecompiledModule::Symbols(
    std::string_view name) const {
  auto iter = symbols_.find(name);
  if (iter == symbols_.end()) { return {}; }
  return iter->second;
}

}  // namespace module
