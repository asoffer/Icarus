#ifndef ICARUS_COMPILER_EMIT_COPY_MOVE_ASSIGNMENT_H
#define ICARUS_COMPILER_EMIT_COPY_MOVE_ASSIGNMENT_H

#include "compiler/compilation_data.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"
#include "type/typed_value.h"
#include "type/variable.h"

namespace compiler {

struct MoveAssignmentEmitter : CompilationDataReference {
  using signature = void(ir::RegOr<ir::addr_t>,
                         type::Typed<ir::PartialResultRef> const &);

  explicit MoveAssignmentEmitter(CompilationDataReference ref)
      : CompilationDataReference(ref) {}

  void operator()(type::Type t, ir::RegOr<ir::addr_t> addr,
                  type::Typed<ir::PartialResultRef> const &from) {
    EmitAssignment(t, addr, from);
  }

  void operator()(auto const *t, ir::RegOr<ir::addr_t> addr,
                  type::Typed<ir::PartialResultRef> const &from) {
    EmitAssignment(t, addr, from);
  }

  void operator()(type::Typed<ir::RegOr<ir::addr_t>> const &addr,
                  type::Typed<ir::PartialResultRef> const &from) {
    EmitAssignment(addr.type(), *addr, from);
  }

  void EmitAssignment(type::Type t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from) {
    t.visit(*this, addr, from);
  }

  void EmitAssignment(type::Array const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Enum const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Flags const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Function const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(
      type::Generic const *t, ir::RegOr<ir::addr_t> addr,
      type::Typed<ir::PartialResultRef> const &from) { /* TODO */
  }
  void EmitAssignment(type::Pointer const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::BufferPointer const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Primitive const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Scope const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Slice const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Struct const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Variable const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
};

struct CopyAssignmentEmitter : CompilationDataReference {
  using signature = void(ir::RegOr<ir::addr_t>,
                         type::Typed<ir::PartialResultRef> const &);

  explicit CopyAssignmentEmitter(CompilationDataReference ref)
      : CompilationDataReference(ref) {}

  void operator()(type::Type t, ir::RegOr<ir::addr_t> addr,
                  type::Typed<ir::PartialResultRef> const &from) {
    EmitAssignment(t, addr, from);
  }

  void operator()(auto const *t, ir::RegOr<ir::addr_t> addr,
                  type::Typed<ir::PartialResultRef> const &from) {
    EmitAssignment(t, addr, from);
  }

  void operator()(type::Typed<ir::RegOr<ir::addr_t>> const &addr,
                  type::Typed<ir::PartialResultRef> const &from) {
    EmitAssignment(addr.type(), *addr, from);
  }

  void EmitAssignment(type::Type t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from) {
    t.visit(*this, addr, from);
  }

  void EmitAssignment(type::Array const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Enum const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Flags const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Function const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(
      type::Generic const *t, ir::RegOr<ir::addr_t> addr,
      type::Typed<ir::PartialResultRef> const &from) { /* TODO */
  }
  void EmitAssignment(type::Pointer const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::BufferPointer const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Primitive const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Scope const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Slice const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Struct const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
  void EmitAssignment(type::Variable const *t, ir::RegOr<ir::addr_t> addr,
                      type::Typed<ir::PartialResultRef> const &from);
};

}  // namespace compiler

#endif  //  ICARUS_COMPILER_EMIT_COPY_MOVE_ASSIGNMENT_H
