#include "ir/module.h"

namespace ic {

Module::Entry const Module::DefaultEntry{
    .qualified_type =
        type::QualifiedType(type::Qualifier::Unqualified(), type::Error)};

Module::Entry const& Module::Lookup(Identifier id) const {
  auto iter = entries_.find(id);
  if (iter == entries_.end()) { return DefaultEntry; }

  return iter->second;
}

void Module::Insert(Identifier id , Module::Entry entry) {
  entries_.emplace(id, std::move(entry));
}

IrFunction& Module::add_function(size_t parameters, size_t returns) {
  auto& f = functions_.emplace_back(parameters, returns);
  FunctionId id(ModuleId::Current(), LocalFunctionId(functions_.size() - 1));
  global_function_registry.Register(id, &f);
  return f;
}

}  // namespace ic
