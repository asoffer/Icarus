#ifndef ICARUS_COMPILER_EMIT_INITIALIZE_H
#define ICARUS_COMPILER_EMIT_INITIALIZE_H

#include "compiler/compilation_data.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"
#include "type/variable.h"

namespace compiler {

// Fills `out` with the default value associated with type `t` if one exists.
// Behavior is only defined if such a default value exists.
void WriteDefaultValueFor(type::Type t, ir::PartialResultBuffer &out);

struct DefaultInitializationEmitter : CompilationDataReference {
  using signature = void(ir::RegOr<ir::addr_t>);

  explicit DefaultInitializationEmitter(CompilationDataReference ref)
      : CompilationDataReference(ref) {}

  void operator()(type::Type t, ir::RegOr<ir::addr_t> addr) {
    EmitInitialize(t, addr);
  }

  void operator()(auto const *t, ir::RegOr<ir::addr_t> addr) {
    EmitInitialize(t, addr);
  }

  void EmitInitialize(type::Type t, ir::RegOr<ir::addr_t> addr) {
    t.visit(*this, addr);
  }

  void EmitInitialize(type::Array const *t, ir::RegOr<ir::addr_t> addr);
  void EmitInitialize(type::Enum const *t, ir::RegOr<ir::addr_t> addr);
  void EmitInitialize(type::Flags const *t, ir::RegOr<ir::addr_t> addr);
  void EmitInitialize(type::Function const *t, ir::RegOr<ir::addr_t> addr);
  void EmitInitialize(type::Pointer const *t, ir::RegOr<ir::addr_t> addr);
  void EmitInitialize(type::BufferPointer const *t, ir::RegOr<ir::addr_t> addr);
  void EmitInitialize(type::Primitive const *t, ir::RegOr<ir::addr_t> addr);
  void EmitInitialize(type::Slice const *t, ir::RegOr<ir::addr_t> addr);
  void EmitInitialize(type::Struct const *t, ir::RegOr<ir::addr_t> addr);
  void EmitInitialize(type::Variable const *t, ir::RegOr<ir::addr_t> addr);
};

struct MoveInitializationEmitter : CompilationDataReference {
  using signature = void(ir::RegOr<ir::addr_t>,
                         ir::PartialResultBuffer const &);

  explicit MoveInitializationEmitter(CompilationDataReference ref)
      : CompilationDataReference(ref) {}

  void operator()(type::Type t, ir::RegOr<ir::addr_t> addr,
                  ir::PartialResultBuffer const &from) {
    EmitInitialize(t, addr, from);
  }

  void operator()(auto const *t, ir::RegOr<ir::addr_t> addr,
                  ir::PartialResultBuffer const &from) {
    EmitInitialize(t, addr, from);
  }

  void operator()(type::Typed<ir::RegOr<ir::addr_t>> const &addr,
                  ir::PartialResultBuffer const &from) {
    EmitInitialize(addr.type(), *addr, from);
  }

  void EmitInitialize(type::Type t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from) {
    t.visit(*this, addr, from);
  }

  void EmitInitialize(type::Array const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Enum const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Flags const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Function const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Pointer const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::BufferPointer const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Primitive const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Slice const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Struct const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Variable const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
};

struct CopyInitializationEmitter : CompilationDataReference {
  using signature = void(ir::RegOr<ir::addr_t>,
                         ir::PartialResultBuffer const &);

  explicit CopyInitializationEmitter(CompilationDataReference ref)
      : CompilationDataReference(ref) {}

  void operator()(type::Type t, ir::RegOr<ir::addr_t> addr,
                  ir::PartialResultBuffer const &from) {
    EmitInitialize(t, addr, from);
  }

  void operator()(auto const *t, ir::RegOr<ir::addr_t> addr,
                  ir::PartialResultBuffer const &from) {
    EmitInitialize(t, addr, from);
  }

  void operator()(type::Typed<ir::RegOr<ir::addr_t>> const &addr,
                  ir::PartialResultBuffer const &from) {
    EmitInitialize(addr.type(), *addr, from);
  }

  void EmitInitialize(type::Type t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from) {
    t.visit(*this, addr, from);
  }

  void EmitInitialize(type::Array const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Enum const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Flags const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Function const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Pointer const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::BufferPointer const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Primitive const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Slice const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Struct const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
  void EmitInitialize(type::Variable const *t, ir::RegOr<ir::addr_t> addr,
                      ir::PartialResultBuffer const &from);
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_EMIT_INITIALIZE_H
