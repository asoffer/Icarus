#include "interpretter/foreign.h"

#include <dlfcn.h>
#include <tuple>
#include <type_traits>
#include <vector>

namespace interpretter {

void CallForeignFn(ir::Foreign const &f, base::untyped_buffer const &arguments,
                   absl::Span<ir::Addr const> ret_slots,
                   base::untyped_buffer *stack) {
  NOT_YET();
}
void *LoadDataSymbol(std::string_view name) {
  return ASSERT_NOT_NULL(dlsym(RTLD_DEFAULT, std::string(name).c_str()));
}
void (*LoadFunctionSymbol(std::string_view name))() {
  // Note: This cast is in general not safe but valid on POSIX compliant
  // systems.
  // TODO: Figure out a portable way of handling this if at all possible.
  return reinterpret_cast<void (*)()>(
      ASSERT_NOT_NULL(dlsym(RTLD_DEFAULT, std::string(name).c_str())));
}

}  // namespace interpretter
