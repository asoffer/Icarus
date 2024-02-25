#ifndef ICARUS_IR_MODULE_H
#define ICARUS_IR_MODULE_H

#include <cstdint>
#include <deque>

#include "absl/container/flat_hash_map.h"
#include "absl/container/inlined_vector.h"
#include "common/any_value.h"
#include "common/identifier.h"
#include "ir/function.h"
#include "ir/scope.h"
#include "jasmin/core/value.h"
#include "type/type.h"

namespace ic {

struct Module {
  AnyValue const& Lookup(Identifier id) const;
  void Insert(Identifier id, AnyValue e);

  IrFunction& insert_initializer();
  void set_initializer(IrFunction& f NTH_ATTRIBUTE(lifetimebound)) {
    init_ = &f;
  }
  IrFunction& initializer() {
    NTH_REQUIRE(init_ != nullptr);
    return *init_;
  }
  IrFunction const& initializer() const {
    NTH_REQUIRE(init_ != nullptr);
    return *init_;
  }

  IrFunction& add_function(size_t parameters, size_t returns);
  IrFunction& add_function(ModuleId id, size_t parameters, size_t returns);

  Scope& add_scope();

  auto const& entries() const { return entries_; }
  auto& entries() { return entries_; }

  auto const& program() const { return program_; }
  auto& program() { return program_; }

 private:
  static AnyValue const DefaultEntry;

  ProgramFragment program_;
  // TODO: Entries might not be constants.
  absl::flat_hash_map<Identifier, AnyValue> entries_;
  IrFunction* init_;
  std::deque<Scope> scopes_;
};

}  // namespace ic

#endif  // ICARUS_IR_MODULE_H
