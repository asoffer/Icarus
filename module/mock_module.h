#ifndef ICARUS_MODULE_MOCK_MODULE_H
#define ICARUS_MODULE_MOCK_MODULE_H

#include "ir/value/module_id.h"
#include "module/module.h"

namespace module {
struct MockModule : Module {
  explicit MockModule(std::string identifier, ir::ModuleId)
      : Module(std::move(identifier)) {}

  MOCK_METHOD(absl::Span<SymbolInformation const>, Symbols,
              (std::string_view name), (const, override));
  MOCK_METHOD(FunctionInformation, Function, (ir::LocalFnId id),
              (const, override));

  MOCK_METHOD(void, SymbolsByName,
              (absl::FunctionRef<void(std::string_view,
                                      absl::Span<SymbolInformation const>)>),
              (const, override));
};

}  // namespace module

#endif  // ICARUS_MODULE_MOCK_MODULE_H
