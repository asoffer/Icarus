#ifndef ICARUS_MODULE_RESOURCES_H
#define ICARUS_MODULE_RESOURCES_H

#include "absl/functional/any_invocable.h"
#include "absl/functional/function_ref.h"
#include "base/ptr_span.h"
#include "diagnostic/consumer/consumer.h"
#include "module/global_function_map.h"
#include "module/global_index_map.h"
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

  size_t imported_modules() const {
    return module_map().imported_modules().size();
  }

  ModuleMap& module_map() { return module_map_; }
  ModuleMap const& module_map() const { return module_map_; }

  GlobalFunctionMap& function_map() { return function_map_; }
  GlobalFunctionMap const& function_map() const { return function_map_; }

  GlobalIndexMap& opaque_map() { return opaque_map_; }
  GlobalIndexMap const& opaque_map() const { return opaque_map_; }

  serialization::UniqueTypeTable const& unique_type_table() const {
    return unique_type_table_;
  }
  serialization::UniqueTypeTable& unique_type_table() {
    return unique_type_table_;
  }

  base::PtrSpan<Module> modules() { return module_map().imported_modules(); }

  Module& module(UniqueId module_id);

  // Returns the `UniqueId` associated with the given `name` if one exists, and
  // returns `UniqueId::Invalid()` otherwise.
  UniqueId TryLoadModuleByName(ModuleName const& name) const;

  diagnostic::DiagnosticConsumer& diagnostic_consumer();

  // Assuming `type` is a type with respect to the type system `from`, returns
  // the corresponding type with respect to the type system `to`.
  core::Type Translate(core::Type type, UniqueId module_id,
                       semantic_analysis::TypeSystem& from,
                       semantic_analysis::TypeSystem& to) const;

  // Assuming `symbol` is defined relative to the module indexed by `from`,
  // returns a `Symbol` equivalent to `symbol` but relative to the primary
  // module.
  Symbol TranslateToPrimary(UniqueId from, Symbol const& symbol);

 private:
  Module primary_module_{UniqueId(""), function_map_};
  serialization::ReadOnlyData read_only_data_;

  GlobalIndexMap opaque_map_;
  GlobalFunctionMap function_map_;
  ModuleMap& module_map_;
  serialization::UniqueTypeTable unique_type_table_;
  diagnostic::DiagnosticConsumer& diagnostic_consumer_;
};

}  // namespace module

#endif  // ICARUS_MODULE_RESOURCES_H
