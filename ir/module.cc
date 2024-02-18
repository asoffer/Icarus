#include "ir/module.h"

#include "absl/strings/str_cat.h"

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
  return add_function(ModuleId::Current(), parameters, returns);
}
IrFunction& Module::add_function(ModuleId id, size_t parameters,
                                 size_t returns) {
  size_t count = program_.function_count();
  auto& f = program_.declare(absl::StrCat("fn.", count), parameters, returns)
                .function;
  NTH_LOG("{}")<<={&f};
  FunctionId fn_id(id, LocalFunctionId(count));
  global_function_registry.Register(fn_id, &f);
  return f;
}

Scope& Module::add_scope() { return scopes_.emplace_back(); }

IrFunction& Module::insert_initializer() {
  init_ = &program_.declare("~", 0, 0).function;
  return *init_;
}

}  // namespace ic
