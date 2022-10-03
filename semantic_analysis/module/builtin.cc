#include "semantic_analysis/module/builtin.h"

#include <dlfcn.h>

#include "base/debug.h"

namespace semantic_analysis {

std::pair<ir::Fn, void (*)()> BuiltinModule::ForeignFunction(std::string name,
                                                             core::Type t) {
  auto [iter, inserted] = foreign_functions_.try_emplace(
      std::pair<std::string, core::Type>(std::move(name), t));
  if (inserted) {
    dlerror();  // Clear previous errors.
    // NOTE: This reinterpret_cast is not allowed according to the C++ Standard,
    // but is guaranteed to be correct on POSIX compliant systems.
    iter->second = reinterpret_cast<void (*)()>(
        dlsym(RTLD_DEFAULT, iter->first.first.c_str()));

    char const* error = dlerror();

    // TODO: Handle errors.
    if (error != nullptr) { NOT_YET(iter->first.first); }
  }

  return std::pair(ir::Fn(ir::ModuleId::Builtin(),
                          ir::LocalFnId(foreign_functions_.index(iter))),
                   iter->second);
}

std::type_identity_t<void (*)()> BuiltinModule::ForeignFunction(
    ir::LocalFnId id) {
  return foreign_functions_.from_index(id.value()).second;
}

}  // namespace semantic_analysis
