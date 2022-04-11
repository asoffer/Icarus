#include "module/shared_context.h"

#include <dlfcn.h>

namespace module {
namespace {

using void_fn_ptr = void (*)();

}  // namespace

void_fn_ptr SharedContext::ForeignFunctionPointer(ir::Fn f) const {
  auto iter           = std::next(foreign_fn_map_.begin(), f.local().value());
  void_fn_ptr& fn_ptr = const_cast<void_fn_ptr&>(iter->second);
  if (not fn_ptr) {
    dlerror();  // Clear previous errors.
    fn_ptr = reinterpret_cast<void_fn_ptr>(
        dlsym(RTLD_DEFAULT, iter->first.first.c_str()));
    char const* err = dlerror();
    ASSERT(err == nullptr);
  }
  return fn_ptr;
}

type::Function const* SharedContext::ForeignFunctionType(ir::Fn f) const {
  return std::next(foreign_fn_map_.begin(), f.local().value())->first.second;
}

}  // namespace module
