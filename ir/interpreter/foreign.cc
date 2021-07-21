#include "ir/interpreter/foreign.h"

#include <dlfcn.h>
#include <ffi.h>

#include <type_traits>
#include <vector>

#include "absl/status/status.h"
#include "absl/status/statusor.h"
#include "base/debug.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"

namespace interpreter {
using void_fn_ptr = void (*)();

absl::StatusOr<void *> LoadDataSymbol(std::string_view name) {
  dlerror();  // Clear previous errors.
  void *result    = dlsym(RTLD_DEFAULT, std::string(name).c_str());
  char const *err = dlerror();
  if (not err) { return result; }
  return absl::NotFoundError(err);
}

absl::StatusOr<void_fn_ptr> LoadFunctionSymbol(std::string_view name) {
  dlerror();  // Clear previous errors.
  auto result = reinterpret_cast<void_fn_ptr>(
      dlsym(RTLD_DEFAULT, std::string(name).c_str()));
  char const *err = dlerror();
  if (not err) { return result; }
  return absl::NotFoundError(err);
}

}  // namespace interpreter
