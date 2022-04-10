#ifndef ICARUS_MODULE_BUILTIN_H
#define ICARUS_MODULE_BUILTIN_H

#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "ir/module.h"
#include "ir/subroutine.h"
#include "ir/value/fn.h"
#include "ir/value/native_fn.h"
#include "module/module.h"

namespace module {

// BuiltinModule:
//
// Represents a builtin module of symbols predefined, rather than those coming
// from a source file.
struct BuiltinModule final : Module {
  static constexpr std::string_view BuiltinIdentifier = "~builtin";

  BuiltinModule() : Module(std::string(BuiltinIdentifier)) {}

  absl::Span<SymbolInformation const> Symbols(
      std::string_view name) const override {
    auto iter = symbols_.find(name);
    if (iter == symbols_.end()) { return {}; }
    return iter->second;
  }

  FunctionInformation Function(uint32_t id) const override {
    auto const &info = module_content_.function(id);
    return FunctionInformation{.type = info.type(), .byte_code = &info.byte_code};
  }

  void insert(std::string_view symbol, SymbolInformation const &info) {
    symbols_.try_emplace(symbol).first->second.push_back(info);
  }

  ir::NativeFn insert_function(ir::Subroutine fn, type::Function const *type,
                               ir::ByteCode byte_code) {
    // TODO: These should be explicitly on InsertFunction.
    ir::NativeFn f = module_content_.InsertFunction(type);
    *f             = std::move(fn);
    f.byte_code()  = std::move(byte_code);
    return f;
  }

 private:
  absl::flat_hash_map<std::string, std::vector<SymbolInformation>> symbols_;
  ir::Module module_content_;
};

}  // namespace module

#endif  // ICARUS_MODULE_BUILTIN_H
