#include "ir/foreign_function.h"

#include <dlfcn.h>

#include <string>

#include "ir/function.h"
#include "ir/function_id.h"
#include "ir/global_function_registry.h"
#include "type/type.h"

namespace ic {

IrFunction const* ForeignFunction(std::string const& name,
                                  type::FunctionType t) {
  auto id = LocalFunctionId(ForeignFunctions().size());
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
  global_function_registry.Register(FunctionId(ModuleId::Foreign(), id), &fn);
  return &fn;
}

}  // namespace ic

