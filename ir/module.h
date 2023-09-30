#ifndef ICARUS_IR_MODULE_H
#define ICARUS_IR_MODULE_H

#include <cstdint>

#include "absl/container/flat_hash_map.h"
#include "absl/container/inlined_vector.h"
#include "jasmin/function.h"
#include "jasmin/instruction.h"
#include "jasmin/instructions/core.h"
#include "jasmin/value.h"
#include "type/type.h"

namespace ic {

struct PrintHelloWorld : jasmin::StackMachineInstruction<PrintHelloWorld> {
  static void execute() { std::cerr << "Hello, world!\n"; }
};

using InstructionSet =
    jasmin::MakeInstructionSet<jasmin::Push, jasmin::Drop, PrintHelloWorld>;
using IrFunction     = jasmin::Function<InstructionSet>;

struct Module {
  struct Entry {
    type::QualifiedType qualified_type =
        type::QualifiedType(type::Qualifier::Unqualified(), type::Error);
    absl::InlinedVector<jasmin::Value, 2> value;
  };
  Entry const& Lookup(uint32_t index) const;
  void Insert(uint32_t index, Entry e);

  constexpr IrFunction& initializer() { return initializer_; }
  constexpr IrFunction const& initializer() const { return initializer_; }

 private:
  static Entry const DefaultEntry;

  absl::flat_hash_map<uint32_t, Entry> entries_;
  IrFunction initializer_{0, 0};
};


}  // namespace ic

#endif  // ICARUS_IR_MODULE_H
