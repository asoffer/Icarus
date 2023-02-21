#ifndef ICARUS_MODULE_RESOURCES_H
#define ICARUS_MODULE_RESOURCES_H

#include "absl/functional/any_invocable.h"
#include "diagnostic/consumer/consumer.h"
#include "module/function_map.h"
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

  Module* LoadPrimary(serialization::Module module);

  // Loads the contents of `module` into a new module and returns a pointer to
  // that module if loading was successful. If unsuccessful, a null pointer is
  // returned.
  Module* LoadFrom(serialization::Module module);

  Module& primary_module() { return primary_module_; }
  Module const& primary_module() const { return primary_module_; }

  FunctionMap& function_map() { return function_map_; }
  FunctionMap const& function_map() const { return function_map_; }

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

  FunctionMap function_map_;
  Module primary_module_{serialization::UniqueModuleId(""), function_map_};
  std::vector<std::unique_ptr<Module>> modules_;
  serialization::ReadOnlyData read_only_data_;

  absl::AnyInvocable<serialization::UniqueModuleId(ModuleName const&) const>
      name_resolver_;
  std::unique_ptr<diagnostic::DiagnosticConsumer> diagnostic_consumer_;
};

}  // namespace module

#endif  // ICARUS_MODULE_RESOURCES_H
