#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <deque>
#include <istream>
#include <optional>
#include <ostream>

#include "data_types/integer.h"
#include "serialization/module_index.h"
#include "semantic_analysis/foreign_function_map.h"
#include "semantic_analysis/instruction_set.h"
#include "semantic_analysis/type_system.h"
#include "serialization/module.pb.h"

namespace module {

struct Module {
  explicit Module(serialization::ReadOnlyData read_only_data = {})
      : read_only_data_(std::move(read_only_data)) {}

  bool Serialize(std::ostream &output) const;
  static std::optional<Module> Deserialize(std::istream &input);

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

  auto const &read_only_data() const { return read_only_data_; }

  semantic_analysis::IrFunction const *function(data_types::Fn fn_id) const {
    if (fn_id.module() == serialization::ModuleIndex::Foreign()) {
      return foreign_function_map_.ForeignFunction(fn_id.local());
    } else if (fn_id.module() == serialization::ModuleIndex(0)) {
      size_t index = fn_id.local().value();
      ASSERT(index < functions_.size());
      return &functions_[index];
    } else {
      NOT_YET();
    }
  }

  std::pair<data_types::Fn, semantic_analysis::IrFunction *> create_function(
      size_t parameters, size_t returns) {
    data_types::Fn fn(serialization::ModuleIndex(0),
                      data_types::LocalFnId(functions_.size()));
    return std::pair(fn, &functions_.emplace_back(parameters, returns));
  }

 private:
  // Accepts two arguments (a slice represented as data followed by length).
  semantic_analysis::IrFunction initializer_{2, 0};

  // The type-system containing all types referenceable in this module.
  mutable semantic_analysis::TypeSystem type_system_;
  semantic_analysis::ForeignFunctionMap foreign_function_map_;
  std::deque<semantic_analysis::IrFunction> functions_;

  // All integer constants used in the module.
  data_types::IntegerTable integer_table_;

  serialization::ReadOnlyData read_only_data_;
};

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
