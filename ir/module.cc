#include "ir/module.h"

#include "absl/strings/str_cat.h"
#include "type/primitive.h"

namespace ic {

AnyValue const Module::DefaultEntry = AnyValue::JustType(type::Error);

AnyValue const& Module::Lookup(Identifier id) const {
  auto iter = entries_.find(id);
  if (iter == entries_.end()) { return DefaultEntry; }

  return iter->second;
}

void Module::Insert(Identifier id, AnyValue value) {
  entries_.emplace(id, std::move(value));
}

IrFunction& Module::add_function(size_t parameters, size_t returns) {
  return add_function(ModuleId::Current(), parameters, returns);
}
IrFunction& Module::add_function(ModuleId id, size_t parameters,
                                 size_t returns) {
  size_t count = program_.function_count();
  return program_.declare(absl::StrCat("fn.", count), parameters, returns)
      .function;
}

Scope& Module::add_scope() { return scopes_.emplace_back(); }

IrFunction& Module::insert_initializer() {
  init_ = &program_.declare("~", 0, 0).function;
  return *init_;
}

}  // namespace ic
