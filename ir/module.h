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
#include "jasmin/core/value.h"
#include "type/type.h"

namespace ic {

struct Module {
  struct Entry {
    type::QualifiedType qualified_type =
        type::QualifiedType(type::Qualifier::Unqualified(), type::Error);
    absl::InlinedVector<jasmin::Value, 2> value;
  };
  Entry const& Lookup(Identifier id) const;
  void Insert(Identifier id, Entry e);

  IrFunction& insert_initializer();
  constexpr IrFunction& initializer() { return *init_; }
  constexpr IrFunction const& initializer() const { return *init_; }

  IrFunction& add_function(size_t parameters, size_t returns);
  IrFunction& add_function(ModuleId id, size_t parameters, size_t returns);

  Scope& add_scope();

  auto const& entries() const { return entries_; }
  auto& entries() { return entries_; }

  auto const& program() const { return program_; }
  auto& program() { return program_; }

 private:
  static Entry const DefaultEntry;

  ProgramFragment program_;
  absl::flat_hash_map<Identifier, Entry> entries_;
  IrFunction* init_;
  std::deque<Scope> scopes_;
};

}  // namespace ic

#endif  // ICARUS_IR_MODULE_H
