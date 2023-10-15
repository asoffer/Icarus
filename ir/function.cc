#include "ir/function.h"

#include <deque>

#include "ir/function_id.h"
#include "ir/global_function_registry.h"
#include "jasmin/value.h"
#include "jasmin/value_stack.h"

namespace ic {
namespace {

std::deque<std::pair<type::FunctionType, IrFunction>> foreign_functions;

}  // namespace

std::deque<std::pair<type::FunctionType, IrFunction>>& ForeignFunctions() {
  return foreign_functions;
}

void RegisterForeignFunction::execute(jasmin::ValueStack& value_stack) {
  char const* data = value_stack.pop<char const*>();
  size_t length    = value_stack.pop<size_t>();
  type::Type t     = value_stack.pop<type::Type>();
  (void)resources.ForeignFunctionIndex(std::string_view(data, length),
                                       t.AsFunction());
  size_t jasmin_parameter_size = 0;
  size_t jasmin_return_size    = 0;
  for (auto const& p : *t.AsFunction().parameters()) {
    jasmin_parameter_size += type::JasminSize(p.type);
  }
  for (type::Type return_type : t.AsFunction().returns()) {
    jasmin_return_size += type::JasminSize(return_type);
  }
  FunctionId id(ModuleId::Foreign(), LocalFunctionId(foreign_functions.size()));
  auto& [fn_type, fn] = foreign_functions.emplace_back(
      std::piecewise_construct, std::forward_as_tuple(t.AsFunction()),
      std::forward_as_tuple(jasmin_parameter_size, jasmin_return_size));
  // TODO: Implement?
  fn.append<jasmin::Return>();
  global_function_registry.Register(id, &fn);
  value_stack.push(&fn);
}

}  // namespace ic
