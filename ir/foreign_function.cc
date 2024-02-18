#include "ir/foreign_function.h"

#include <dlfcn.h>

#include <string>
#include <string_view>

#include "common/foreign_function.h"
#include "common/resources.h"
#include "ir/function.h"
#include "ir/function_id.h"
#include "ir/global_function_registry.h"
#include "jasmin/core/instruction.h"
#include "type/function.h"
#include "type/primitive.h"

namespace ic {
namespace {

nth::flyweight_map<std::pair<StringLiteral, type::FunctionType>,
                   std::pair<type::FunctionType, IrFunction const*>>
    foreign_functions;

nth::flyweight_map<std::pair<size_t, type::PointerType>,
                   std::pair<type::PointerType, void*>>
    foreign_pointers;

ProgramFragment global_program;

}  // namespace

IrFunction const& InsertForeignFunction(StringLiteral name,
                                        type::FunctionType t, bool) {
  ForeignFunction f(name, t);
  auto [iter, inserted] =
      foreign_functions.try_emplace(std::make_pair(name, t), t, nullptr);
  if (not inserted) { return *iter->second.second; }

  size_t jasmin_parameter_size = 0;
  size_t jasmin_return_size    = 0;
  for (auto const& p : *t.parameters()) {
    jasmin_parameter_size += type::JasminSize(p.type);
  }
  for (type::Type return_type : t.returns()) {
    jasmin_return_size += type::JasminSize(return_type);
  }
  std::string name_str(name.str());
  auto& fn = global_program
                 .declare(name_str, jasmin_parameter_size, jasmin_return_size)
                 .function;

  // TODO: Always implement?
  fn.append<InvokeForeignFunction>(
      {.parameters = static_cast<uint32_t>(t.parameters().size()),
       .returns    = static_cast<uint32_t>(t.returns().size())},
      t, f.function().ptr());

  fn.append<jasmin::Return>();
  global_function_registry.Register(
      FunctionId(ModuleId::Foreign(),
                 LocalFunctionId(foreign_functions.index(iter))),
      &fn);
  iter->second.second = &fn;
  return fn;
}

void* InsertForeignPointer(std::string_view name, type::PointerType t) {
  auto [iter, inserted] = foreign_pointers.try_emplace(
      std::make_pair(resources.StringLiteralIndex(name), t), t, nullptr);
  if (not inserted) { return iter->second.second; }

  dlerror();  // Clear existing errors.
  iter->second.second = dlsym(RTLD_DEFAULT, std::string(name).c_str());
  char const* error   = dlerror();
  if (error != nullptr) { NTH_UNIMPLEMENTED("{}") <<= {error}; }

  global_pointer_registry.Register(foreign_pointers.index(iter),
                                   iter->second.second);
  return iter->second.second;
}

}  // namespace ic
