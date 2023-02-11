#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <deque>
#include <istream>
#include <optional>
#include <ostream>

#include "data_types/integer.h"
#include "module/symbol.h"
#include "semantic_analysis/foreign_function_map.h"
#include "semantic_analysis/instruction_set.h"
#include "semantic_analysis/type_system.h"
#include "serialization/module.pb.h"
#include "serialization/module_index.h"
#include "serialization/module_map.h"

namespace module {

struct Module {
  explicit Module(serialization::UniqueModuleId id,
                  serialization::ReadOnlyData read_only_data = {})
      : id_(std::move(id)), read_only_data_(std::move(read_only_data)) {}

  bool Serialize(std::ostream &output) const;
  static bool DeserializeInto(serialization::Module proto, Module &module);

  semantic_analysis::IrFunction &initializer() { return initializer_; }
  semantic_analysis::IrFunction const &initializer() const {
    return initializer_;
  }

  semantic_analysis::TypeSystem &type_system() const { return type_system_; }

  semantic_analysis::ForeignFunctionMap const &foreign_function_map() const {
    return foreign_function_map_;
  }

  semantic_analysis::ForeignFunctionMap &foreign_function_map() {
    return foreign_function_map_;
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

  void Export(std::string_view name, core::Type t,
              semantic_analysis::IrFunction const *f) {
    Export(name, Symbol(TypedFunction{
                     .type     = t,
                     .function = find(f),
                 }));
  }

  auto const &read_only_data() const { return read_only_data_; }

  // TODO: Figure out a better way to do foreign functions. The're shared but
  // maybe you don't want them to have their own module index since the value is
  // actually dependent on the module you're loading them from. Once you do that
  // this function can be modified to only accept a `LocalFnId`. Maybe foreign
  // functions are just intermixed with normal functions? do they need special
  // treatment?
  semantic_analysis::IrFunction const *function(
      data_types::LocalFnId fn_id) const {
    if (fn_id.foreign()) {
      return foreign_function_map_.ForeignFunction(fn_id);
    } else {
      size_t index = fn_id.value();
      ASSERT(index < functions_.size());
      return &functions_[index];
    }
  }

  std::pair<data_types::Fn, semantic_analysis::IrFunction *> create_function(
      size_t parameters, size_t returns) {
    data_types::Fn fn(serialization::ModuleIndex(0),
                      data_types::LocalFnId(functions_.size()));
    auto &f = functions_.emplace_back(parameters, returns);
    function_indices_.emplace(&f, functions_.size() - 1);
    return std::pair(fn, &f);
  }

  serialization::ModuleMap const &map() const { return module_map_; }
  serialization::ModuleMap &map() { return module_map_; }

 private:
  serialization::UniqueModuleId id_;

  data_types::LocalFnId find(semantic_analysis::IrFunction const *f) {
    auto iter = function_indices_.find(f);
    if (iter != function_indices_.end()) {
      return data_types::LocalFnId(iter->second);
    }
    NOT_YET("Return a foreign function");
  }

  // Accepts two arguments (a slice represented as data followed by length).
  semantic_analysis::IrFunction initializer_{2, 0};

  absl::flat_hash_map<std::string, std::vector<Symbol>> exported_symbols_;

  // The type-system containing all types referenceable in this module.
  mutable semantic_analysis::TypeSystem type_system_;
  semantic_analysis::ForeignFunctionMap foreign_function_map_;

  // TODO: Can we turn this into an nth::flyweight_set?
  std::deque<semantic_analysis::IrFunction> functions_;
  absl::flat_hash_map<semantic_analysis::IrFunction const *, size_t>
      function_indices_;

  // All integer constants used in the module.
  data_types::IntegerTable integer_table_;

  serialization::ReadOnlyData read_only_data_;
  serialization::ModuleMap module_map_;
};

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
