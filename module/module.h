#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <deque>
#include <istream>
#include <optional>
#include <ostream>

#include "base/ptr_span.h"
#include "data_types/integer.h"
#include "module/global_function_map.h"
#include "module/global_module_map.h"
#include "module/symbol.h"
#include "semantic_analysis/type_system.h"
#include "serialization/foreign_symbol_map.h"
#include "serialization/module.pb.h"
#include "serialization/module_index.h"
#include "serialization/read_only_data.h"
#include "vm/function.h"
#include "vm/function_table.h"

namespace module {

struct Module {
  explicit Module(serialization::UniqueModuleId id,
                  GlobalFunctionMap &function_map)
      : id_(std::move(id)), function_table_(function_map) {}

  bool Serialize(std::ostream &output, GlobalFunctionMap &function_map) const;
  static bool DeserializeInto(serialization::Module const &proto,
                              base::PtrSpan<Module const> dependencies,
                              serialization::ModuleIndex module_index,
                              Module &module, GlobalModuleMap &module_map,
                              GlobalFunctionMap &function_map);

  vm::Function &initializer() { return initializer_; }
  vm::Function const &initializer() const { return initializer_; }

  semantic_analysis::TypeSystem &type_system() const { return type_system_; }

  serialization::ForeignSymbolMap const &foreign_symbol_map() const {
    return foreign_symbol_map_;
  }
  serialization::ForeignSymbolMap &foreign_symbol_map() {
    return foreign_symbol_map_;
  }

  data_types::IntegerTable &integer_table() { return integer_table_; }
  data_types::IntegerTable const &integer_table() const {
    return integer_table_;
  }

  std::span<Symbol const> LoadSymbols(std::string_view name) const {
    auto iter = exported_symbols_.find(name);
    if (iter == exported_symbols_.end()) { return {}; }
    return iter->second;
  }

  void Export(std::string_view name, Symbol s) {
    exported_symbols_[name].push_back(std::move(s));
  }

  void Export(std::string_view name, core::Type t, vm::Function const *f) {
    Export(name, Symbol(TypedFunction{
                     .type     = t,
                     .function = function_table().find(f),
                 }));
  }

  auto const &read_only_data() const { return read_only_data_; }
  auto &read_only_data() { return read_only_data_; }
  vm::FunctionTable const &function_table() const { return function_table_; }
  vm::FunctionTable &function_table() { return function_table_; }

  std::pair<serialization::FunctionIndex, vm::Function const *> Wrap(
      vm::Function const *f);

 private:
  serialization::UniqueModuleId id_;

  // Accepts two arguments (a slice represented as data followed by length).
  vm::Function initializer_{2, 0};

  absl::flat_hash_map<std::string, std::vector<Symbol>> exported_symbols_;

  absl::flat_hash_map<
      vm::Function const *,
      std::pair<serialization::FunctionIndex, vm::Function const *>>
      wrappers_;

  // The type-system containing all types referenceable in this module.
  mutable semantic_analysis::TypeSystem type_system_;
  // TODO: Mutable because jasmin doesn't correctly pass const qualifiers
  // through `get`. Remove when that's fixed.
  mutable serialization::ForeignSymbolMap foreign_symbol_map_{&type_system_};
  mutable vm::FunctionTable function_table_;

  // All integer constants used in the module.
  data_types::IntegerTable integer_table_;

  mutable serialization::ReadOnlyData read_only_data_;
};

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
