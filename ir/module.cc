#include "ir/module.h"

namespace ic {

Module::Entry const Module::DefaultEntry{
    .qualified_type =
        type::QualifiedType(type::Qualifier::Unqualified(), type::Error)};

Module::Entry const& Module::Lookup(uint32_t index) const {
  auto iter = entries_.find(index);
  if (iter == entries_.end()) { return DefaultEntry; }

  return iter->second;
}

void Module::Insert(uint32_t index, Module::Entry entry) {
  entries_.emplace(index, std::move(entry));
}

}  // namespace ic
