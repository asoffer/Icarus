#ifndef ICARUS_MODULE_RESOURCES_H
#define ICARUS_MODULE_RESOURCES_H

#include "absl/functional/any_invocable.h"
#include "base/ptr_span.h"
#include "diagnostic/consumer/consumer.h"
#include "module/global_function_map.h"
#include "module/global_module_map.h"
#include "module/module.h"
#include "module/module_name.h"
#include "serialization/read_only_data.h"

namespace module {

struct Resources {
  explicit Resources(
      serialization::UniqueModuleId const& id,
      absl::AnyInvocable<serialization::UniqueModuleId(ModuleName const&) const>
          name_resolver,
      std::unique_ptr<diagnostic::DiagnosticConsumer> diagnostic_consumer);

  Module& AllocateModule(serialization::UniqueModuleId const& id);

  Module& primary_module() { return primary_module_; }
  Module const& primary_module() const { return primary_module_; }

  size_t imported_modules() const { return modules_.size(); }

  GlobalFunctionMap& function_map() { return function_map_; }
  GlobalFunctionMap const& function_map() const { return function_map_; }

  GlobalModuleMap& module_map() { return module_map_; }
  GlobalModuleMap const& module_map() const { return module_map_; }

  base::PtrSpan<Module> modules() { return modules_; }

  Module& module(serialization::ModuleIndex index) {
    return *modules_[index.value()];
  }

  // Returns the `serialization::ModuleIndex` associated with the given `name`
  // if one exists, and returns `serialization::ModuleIndex::Invalid()`
  // otherwise.
  serialization::ModuleIndex TryLoadModuleByName(ModuleName const& name) const;

  diagnostic::DiagnosticConsumer& diagnostic_consumer();

  // Assuming `type` is a type with respect to the type system `from`, returns
  // the corresponding type with respect to the type system `to`.
  core::Type Translate(core::Type type, semantic_analysis::TypeSystem& from,
                       semantic_analysis::TypeSystem& to) const;

  // Assuming `symbol` is defined relative to the module indexed by `from`,
  // returns a `Symbol` equivalent to `symbol` but relative to the primary
  // module.
  Symbol TranslateToPrimary(serialization::ModuleIndex from,
                            Symbol const& symbol);

 private:
  explicit Resources() = default;

  Module primary_module_{serialization::UniqueModuleId(""), function_map_};
  std::vector<std::unique_ptr<Module>> modules_;
  serialization::ReadOnlyData read_only_data_;

  GlobalFunctionMap function_map_;
  GlobalModuleMap module_map_;
  absl::AnyInvocable<serialization::UniqueModuleId(ModuleName const&) const>
      name_resolver_;
  std::unique_ptr<diagnostic::DiagnosticConsumer> diagnostic_consumer_;
};


}  // namespace module

#endif  // ICARUS_MODULE_RESOURCES_H
