#include "ir/foreign_function.h"

#include <dlfcn.h>

#include <string>

#include "common/resources.h"
#include "ir/function.h"
#include "type/type.h"

namespace ic {

IrFunction const* ForeignFunction(std::string const& name,
                                  type::FunctionType t) {
  auto& fn =
      ForeignFunctions()
          .emplace_back(
              std::piecewise_construct, std::forward_as_tuple(t),
              std::forward_as_tuple(t.parameters().size(), t.returns().size()))
          .second;
  dlerror();  // Clear existing errors.
  void* result      = dlsym(RTLD_DEFAULT, name.c_str());
  char const* error = dlerror();
  if (error != nullptr) {
    NTH_LOG("{}") <<= {error};
    // TODO: Report the error.
    return nullptr;
  }
  fn.append<InvokeForeignFunction>(t, result);
  fn.append<jasmin::Return>();
  return &fn;
}

}  // namespace ic

