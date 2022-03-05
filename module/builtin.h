#ifndef ICARUS_MODULE_BUILTIN_H
#define ICARUS_MODULE_BUILTIN_H

#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "module/module.h"

namespace module {

// BuiltinModule:
//
// Represents a builtin module of symbols predefined, rather than those coming
// from a source file.
struct BuiltinModule final : Module {
  BuiltinModule() : Module("~builtin") {}

  absl::Span<SymbolInformation const> Exported(std::string_view name) override {
    auto iter = symbols_.find(name);
    if (iter == symbols_.end()) { return {}; }
    return iter->second;
  }

  void insert(std::string_view symbol, SymbolInformation const &info) {
    symbols_.try_emplace(symbol).first->second.push_back(info);
  }

 private:
  absl::flat_hash_map<std::string, std::vector<SymbolInformation>> symbols_;
};

}  // namespace module

#endif  // ICARUS_MODULE_BUILTIN_H
