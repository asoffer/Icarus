#ifndef ICARUS_MODULE_RESOURCES_H
#define ICARUS_MODULE_RESOURCES_H

#include "absl/functional/any_invocable.h"
#include "absl/functional/function_ref.h"
#include "base/ptr_span.h"
#include "diagnostic/consumer/consumer.h"
#include "module/global_function_map.h"
#include "module/module.h"
#include "module/module_map.h"
#include "module/module_name.h"
#include "serialization/read_only_data.h"
#include "serialization/unique_type_table.h"

namespace module {

struct Resources {
  explicit Resources(UniqueId id, ModuleMap& module_map,
                     diagnostic::DiagnosticConsumer& diagnostic_consumer);

  Module& primary_module() { return primary_module_; }
  Module const& primary_module() const { return primary_module_; }

  ModuleMap& module_map() { return module_map_; }
  ModuleMap const& module_map() const { return module_map_; }

  GlobalFunctionMap& function_map() { return function_map_; }
  GlobalFunctionMap const& function_map() const { return function_map_; }

  serialization::UniqueTypeTable const& unique_type_table() const {
    return unique_type_table_;
  }
  serialization::UniqueTypeTable& unique_type_table() {
    return unique_type_table_;
  }

  Module& module(UniqueId module_id);

  diagnostic::DiagnosticConsumer& diagnostic_consumer();

 private:
  Module primary_module_{UniqueId(""), function_map_};
  serialization::ReadOnlyData read_only_data_;

  GlobalFunctionMap function_map_;
  ModuleMap& module_map_;
  serialization::UniqueTypeTable unique_type_table_;
  diagnostic::DiagnosticConsumer& diagnostic_consumer_;
};

}  // namespace module

#endif  // ICARUS_MODULE_RESOURCES_H