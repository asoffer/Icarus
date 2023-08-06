#include "module/module.h"

namespace module {

std::pair<serialization::FunctionIndex, vm::Function const*> Module::Wrap(
    vm::Function const* f) {
  auto [iter, inserted] = wrappers_.try_emplace(f);
  if (inserted) {
    auto result =
        function_table().emplace(f->parameter_count(), f->return_count(), id());
    auto [fn_index, fn_ptr] = result;
    fn_ptr->AppendPushFunction(f);
    fn_ptr->AppendCall();
    fn_ptr->AppendReturn();
    iter->second = result;
  }
  return iter->second;
}

}  // namespace module
