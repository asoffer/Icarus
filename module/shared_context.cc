#include "module/shared_context.h"

#include <dlfcn.h>

namespace module {
namespace {

using void_fn_ptr = void (*)();

}  // namespace

void_fn_ptr SharedContext::ForeignFunctionPointer(ir::Fn f) const {
  NOT_YET();
  // auto& fn_ptr = f.foreign().value_->second;
  // if (not fn_ptr) {
  //   dlerror();  // Clear previous errors.
  //   fn_ptr = reinterpret_cast<void_fn_ptr>(
  //       dlsym(RTLD_DEFAULT, f.foreign().value_->first.first.c_str()));
  //   char const* err = dlerror();
  //   ASSERT(err == nullptr);
  // }
  // return fn_ptr;
}

}  // namespace module
