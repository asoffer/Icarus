#ifndef ICARUS_MODULE_IMPORTER_H
#define ICARUS_MODULE_IMPORTER_H

#include <string>
#include <string_view>
#include <vector>

#include "absl/types/span.h"
#include "ir/value/module_id.h"
#include "module/module.h"

// TODO: We should be able to have a diagnostic conusmer without specifying
// source so that file importer can be agnostic to the diagnostic consuming
// mechanism.
namespace module {

// `Importer` is responsible for scheduling any imports requested from an
// `import` expression.
struct Importer {
  virtual ~Importer() {}
  virtual ir::ModuleId Import(Module const* requestor,
                              std::string_view module_locator) = 0;
  virtual Module& get(ir::ModuleId id)                         = 0;

  bool SetImplicitlyEmbeddedModules(
      absl::Span<std::string const> module_locators);

  absl::Span<ir::ModuleId const> implicitly_embedded_modules() const {
    return embedded_module_ids_;
  }

 private:
  std::vector<ir::ModuleId> embedded_module_ids_;
};

}  // namespace module

#endif  // ICARUS_MODULE_IMPORTER_H
