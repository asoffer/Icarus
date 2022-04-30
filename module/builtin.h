#ifndef ICARUS_MODULE_BUILTIN_H
#define ICARUS_MODULE_BUILTIN_H

#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/functional/function_ref.h"
#include "absl/types/span.h"
#include "base/any_invocable.h"
#include "ir/module.h"
#include "ir/subroutine.h"
#include "ir/value/fn.h"
#include "module/module.h"

namespace module {

// BuiltinModule:
//
// Represents a builtin module of symbols predefined, rather than those coming
// from a source file.
struct BuiltinModule final : Module {
  static constexpr std::string_view BuiltinIdentifier = "~builtin";

  BuiltinModule(base::any_invocable<ir::ByteCode(ir::Subroutine const &)>
                    byte_code_emitter)
      : Module(std::string(BuiltinIdentifier)),
        module_content_(ir::ModuleId::Builtin(), std::move(byte_code_emitter)) {
  }

  absl::Span<SymbolInformation const> Symbols(
      std::string_view name) const override {
    auto iter = symbols_.find(name);
    if (iter == symbols_.end()) { return {}; }
    return iter->second;
  }

  FunctionInformation Function(ir::LocalFnId id) const override {
    auto const &info = module_content_.function(id);
    return FunctionInformation{.type       = info.type(),
                               .byte_code  = info.byte_code,
                               .subroutine = &info.subroutine};
  }

  void insert(std::string_view symbol, SymbolInformation const &info) {
    symbols_.try_emplace(symbol).first->second.push_back(info);
  }

  ir::Fn insert_function(
      type::Function const *fn_type,
      absl::FunctionRef<void(ir::Subroutine &)> initialize_subroutine) {
    return module_content_.InsertFunction(fn_type, initialize_subroutine);
  }

 private:
  absl::flat_hash_map<std::string, std::vector<SymbolInformation>> symbols_;
  ir::Module module_content_;
};

}  // namespace module

#endif  // ICARUS_MODULE_BUILTIN_H
