#ifndef ICARUS_PRECOMPILED_MODULE_H
#define ICARUS_PRECOMPILED_MODULE_H

#include <string>

#include "absl/status/statusor.h"
#include "base/flyweight_map.h"
#include "module/map.h"
#include "module/module.h"
#include "module/shared_context.h"
#include "precompiled/module.pb.h"
#include "type/system.h"
#include "type/type.h"

namespace precompiled {

// PrecompiledModule:
//
// Represents a module that has already been compiled, rather than those coming
// from a source file.
struct PrecompiledModule final : module::Module {
  explicit PrecompiledModule(std::string identifier, ir::ModuleId,
                             module::SharedContext& shared_context);

  static absl::StatusOr<
      std::pair<ir::ModuleId, precompiled::PrecompiledModule const*>>
  Load(std::string const& import_name, module::ModuleMap const& module_map,
       module::SharedContext& shared_context);

  absl::Span<module::Module::SymbolInformation const> Symbols(
      std::string_view name) const override;

  module::Module::FunctionInformation Function(
      ir::LocalFnId id) const override;

 private:
  static absl::StatusOr<std::pair<ir::ModuleId, PrecompiledModule*>> Make(
      std::string_view label, ModuleProto const& module_proto,
      module::ModuleMap const& module_map, module::SharedContext& context);

  ModuleProto proto_;
  [[maybe_unused]] module::SharedContext& shared_context_;
  absl::flat_hash_map<std::string,
                      std::vector<module::Module::SymbolInformation>>
      symbols_;
  std::vector<ir::Subroutine> subroutines_;
};

}  // namespace precompiled

#endif  // ICARUS_PRECOMPILED_MODULE_H
