#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <deque>
#include <istream>
#include <optional>
#include <ostream>

#include "base/ptr_span.h"
#include "module/global_function_map.h"
#include "module/symbol.h"
#include "module/unique_id.h"
#include "semantic_analysis/type_system.h"
#include "serialization/foreign_symbol_map.h"
#include "serialization/module.pb.h"
#include "serialization/read_only_data.h"
#include "serialization/unique_type_table.h"
#include "vm/function.h"
#include "vm/function_table.h"

namespace module {

inline GlobalFunctionMap global_hack;

struct Module {
  explicit Module(UniqueId id)
      : id_(std::move(id)), function_table_(global_hack) {}
  explicit Module(UniqueId id, GlobalFunctionMap &function_map)
      : id_(std::move(id)), function_table_(function_map) {}

  vm::Function &initializer() { return initializer_; }
  vm::Function const &initializer() const { return initializer_; }

  semantic_analysis::TypeSystem &type_system() const { return type_system_; }

  serialization::ForeignSymbolMap const &foreign_symbol_map() const {
    return foreign_symbol_map_;
  }
  serialization::ForeignSymbolMap &foreign_symbol_map() {
    return foreign_symbol_map_;
  }

  std::span<Symbol const> LoadSymbols(std::string_view name) const {
    auto iter = exported_symbols_.find(name);
    if (iter == exported_symbols_.end()) { return {}; }
    return iter->second;
  }

  void Export(std::string_view name, Symbol s) {
    exported_symbols_[name].push_back(std::move(s));
  }

  void Export(std::string_view name, core::Type t, jasmin::Value v) {
    Export(name, Symbol(TypedValue{.type = t, .value = v}));
  }

  void Export(std::string_view name, core::Type t, vm::Function const *f) {
    Export(name, Symbol(TypedFunction{
                     .type     = t,
                     .function = function_table().find(f),
                 }));
  }

  UniqueId id() const { return id_; }

  auto const &read_only_data() const { return read_only_data_; }
  auto &read_only_data() { return read_only_data_; }
  vm::FunctionTable const &function_table() const { return function_table_; }
  vm::FunctionTable &function_table() { return function_table_; }

  std::pair<module::LocalFnId, vm::Function const *> Wrap(
      vm::Function const *f);

  auto &exported_symbols() { return exported_symbols_; }
  auto const &exported_symbols() const { return exported_symbols_; }

 private:
  UniqueId id_;

  vm::Function initializer_{0, 0};

  absl::flat_hash_map<std::string, std::vector<Symbol>> exported_symbols_;

  absl::flat_hash_map<vm::Function const *,
                      std::pair<LocalFnId, vm::Function const *>>
      wrappers_;

  // The type-system containing all types referenceable in this module.
  mutable semantic_analysis::TypeSystem type_system_;
  // TODO: Mutable because jasmin doesn't correctly pass const qualifiers
  // through `get`. Remove when that's fixed.
  mutable serialization::ForeignSymbolMap foreign_symbol_map_{&type_system_};
  mutable vm::FunctionTable function_table_;

  mutable serialization::ReadOnlyData read_only_data_;
};

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
