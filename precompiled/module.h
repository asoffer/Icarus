#ifndef ICARUS_PRECOMPILED_MODULE_H
#define ICARUS_PRECOMPILED_MODULE_H

#include <string>

#include "absl/status/statusor.h"
#include "base/flyweight_map.h"
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
                             ModuleProto module_proto,
                             module::SharedContext& shared_context);

  static absl::StatusOr<
      std::pair<ir::ModuleId, precompiled::PrecompiledModule const*>>
  Load(std::string const& file_name, absl::Span<std::string const> lookup_paths,
       absl::flat_hash_map<std::string, std::string> const& module_map,
       module::SharedContext& shared_context);

  absl::Span<module::Module::SymbolInformation const> Symbols(
      std::string_view name) const override;

  module::Module::FunctionInformation Function(
      ir::LocalFnId id) const override {
    ASSERT(id.value() < proto_.function().size());
    return module::Module::FunctionInformation{
        .type      = nullptr,
        .byte_code = ir::ByteCodeView(proto_.function(id.value()).byte_code()),
    };
  }

 private:
  static absl::StatusOr<std::pair<ir::ModuleId, PrecompiledModule const*>> Make(
      std::string const& file_name, module::SharedContext& context);

  ModuleProto proto_;
  [[maybe_unused]] module::SharedContext& shared_context_;
  absl::flat_hash_map<std::string,
                      std::vector<module::Module::SymbolInformation>>
      symbols_;
};

}  // namespace precompiled

#endif  // ICARUS_PRECOMPILED_MODULE_H
