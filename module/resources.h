#ifndef ICARUS_MODULE_RESOURCES_H
#define ICARUS_MODULE_RESOURCES_H

#include "absl/functional/any_invocable.h"
#include "diagnostic/consumer/consumer.h"
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

  // Attempts to deserialize `module` and populate the primary module in a
  // `Resources` object. Returns the object if successful. Returns
  // `std::nullopt` otherwise.
  static std::optional<Resources> LoadPrimary(serialization::Module module);

  // Loads the contents of `module` into a new module and returns a pointer to
  // that module if loading was successful. If unsuccessful, a null pointer is
  // returned.
  Module* LoadFrom(serialization::Module module);

  Module& primary_module() { return primary_module_; }
  Module const& primary_module() const { return primary_module_; }
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

  data_types::Fn TranslateToPrimary(data_types::Fn f);

 private:
  explicit Resources() = default;

  void Populate(serialization::UniqueModuleId const& id,
                std::unique_ptr<Module> m);

  Module primary_module_{serialization::UniqueModuleId("")};
  std::vector<std::unique_ptr<Module>> modules_;
  serialization::ReadOnlyData read_only_data_;

  absl::AnyInvocable<serialization::UniqueModuleId(ModuleName const&) const>
      name_resolver_;
  std::unique_ptr<diagnostic::DiagnosticConsumer> diagnostic_consumer_;
};

}  // namespace module

#endif  // ICARUS_MODULE_RESOURCES_H
