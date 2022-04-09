#include "ir/instruction/foreign.h"

#include <dlfcn.h>

#include "absl/status/status.h"

namespace ir {

absl::StatusOr<void *> LoadDataSymbol(std::string const &name) {
  dlerror();  // Clear previous errors.
  void *result    = dlsym(RTLD_DEFAULT, name.c_str());
  char const *err = dlerror();
  if (not err) { return result; }
  return absl::NotFoundError(err);
}

absl::StatusOr<void (*)()> LoadFunctionSymbol(std::string const &name) {
  dlerror();  // Clear previous errors.
  auto result = reinterpret_cast<void (*)()>(dlsym(RTLD_DEFAULT, name.c_str()));
  char const *err = dlerror();
  if (not err) { return result; }
  return absl::NotFoundError(err);
}

}  // namespace ir
