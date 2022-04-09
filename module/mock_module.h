#ifndef ICARUS_MODULE_MOCK_MODULE_H
#define ICARUS_MODULE_MOCK_MODULE_H

#include "module/module.h"

namespace module  {
struct MockModule : Module {
  explicit MockModule(std::string identifier) : Module(std::move(identifier)) {}

  MOCK_METHOD(absl::Span<SymbolInformation const>, Symbols,
              (std::string_view name), (const, override));
};

}  // namespace module

#endif  // ICARUS_MODULE_MOCK_MODULE_H
