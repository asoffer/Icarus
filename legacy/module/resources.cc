#include "module/resources.h"

#include "nth/debug/debug.h"

namespace module {

Resources::Resources(UniqueId id, ModuleMap &module_map,
                     diagnostic::DiagnosticConsumer &diagnostic_consumer)
    : primary_module_(id, function_map_),
      module_map_(module_map),
      diagnostic_consumer_(diagnostic_consumer) {}

Module &Resources::module(UniqueId module_id) {
  for (auto *m : module_map().imported_modules()) {
    if (m->id() == module_id) { return *m; }
  }
  NTH_UNREACHABLE("Unable to find module {}") <<= {module_id};
}

diagnostic::DiagnosticConsumer &Resources::diagnostic_consumer() {
  return diagnostic_consumer_;
}

}  // namespace module
