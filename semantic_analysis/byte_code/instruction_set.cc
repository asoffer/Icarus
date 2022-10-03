#include "semantic_analysis/byte_code/instruction_set.h"

#include "ir/value/fn.h"

namespace semantic_analysis {

void BuiltinForeign::execute(jasmin::ValueStack& value_stack, core::Type t,
                             BuiltinModule* module) {
  size_t length        = value_stack.pop<size_t>();
  char const* data     = value_stack.pop<char const*>();
  auto [fn_id, fn_ptr] = module->ForeignFunction(std::string(data, length), t);
  if (fn_ptr == nullptr) { NOT_YET(); }
  value_stack.push(fn_id);
}

}  // namespace semantic_analysis
