#ifndef ICARUS_MODULE_PRECOMPILED_H
#define ICARUS_MODULE_PRECOMPILED_H

#include <string>

#include "absl/status/statusor.h"
#include "base/flyweight_map.h"
#include "module/module.h"
#include "module/module.pb.h"
#include "module/shared_context.h"
#include "type/type.h"

namespace module {

// PrecompiledModule:
//
// Represents a module that has already been compiled, rather than those coming
// from a source file.
struct PrecompiledModule final : Module {
  explicit PrecompiledModule(std::string identifier, ir::ModuleId,
                             module_proto::Module module_proto)
      : Module(std::move(identifier)), proto_(std::move(module_proto)) {}

  static absl::StatusOr<std::pair<ir::ModuleId, PrecompiledModule const*>> Make(
      std::string const& file_name, SharedContext& context);

  absl::Span<SymbolInformation const> Symbols(
      std::string_view name) const override;

  FunctionInformation Function(ir::LocalFnId id) const override {
    ASSERT(id.value() < proto_.function().size());
    return FunctionInformation{
        .type      = nullptr,
        .byte_code = ir::ByteCodeView(proto_.function(id.value()).byte_code()),
    };
  }

 private:
  module_proto::Module proto_;
};

}  // namespace module

#endif  // ICARUS_MODULE_PRECOMPILED_H
