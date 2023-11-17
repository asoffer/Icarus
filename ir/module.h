#ifndef ICARUS_IR_MODULE_H
#define ICARUS_IR_MODULE_H

#include <cstdint>
#include <deque>

#include "absl/container/flat_hash_map.h"
#include "absl/container/inlined_vector.h"
#include "common/identifier.h"
#include "ir/function.h"
#include "ir/global_function_registry.h"
#include "ir/scope.h"
#include "jasmin/value.h"
#include "type/type.h"

namespace ic {

struct Module {
  explicit Module() { functions_.emplace_back(0, 0); }

  struct Entry {
    type::QualifiedType qualified_type =
        type::QualifiedType(type::Qualifier::Unqualified(), type::Error);
    absl::InlinedVector<jasmin::Value, 2> value;
  };
  Entry const& Lookup(Identifier id) const;
  void Insert(Identifier id, Entry e);

  constexpr IrFunction& initializer() { return functions_[0]; }
  constexpr IrFunction const& initializer() const { return functions_[0]; }

  constexpr std::deque<IrFunction>& functions() { return functions_; }
  constexpr std::deque<IrFunction> const& functions() const {
    return functions_;
  }

  IrFunction& add_function(size_t parameters, size_t returns);
  IrFunction& add_function(ModuleId id, size_t parameters, size_t returns);

  Scope& add_scope();

  auto const& entries() const { return entries_; }

 private:
  static Entry const DefaultEntry;

  absl::flat_hash_map<Identifier, Entry> entries_;
  std::deque<IrFunction> functions_;
  std::deque<Scope> scopes_;
};

}  // namespace ic

#endif  // ICARUS_IR_MODULE_H
