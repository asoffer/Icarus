#ifndef ICARUS_COMPILER_EMIT_DESTROY_H
#define ICARUS_COMPILER_EMIT_DESTROY_H

#include "absl/cleanup/cleanup.h"
#include "compiler/compilation_data.h"
#include "compiler/emit/common.h"
#include "compiler/instructions.h"
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
#include "type/variable.h"

namespace compiler {

struct DestructionEmitter : CompilationDataReference {
  using signature = void(ir::RegOr<ir::addr_t>);

  explicit DestructionEmitter(CompilationDataReference ref)
      : CompilationDataReference(ref) {}

  void operator()(type::Type t, ir::RegOr<ir::addr_t> addr) {
    EmitDestroy(t, addr);
  }
  void operator()(auto const *t, ir::RegOr<ir::addr_t> addr) {
    EmitDestroy(t, addr);
  }
  void EmitDestroy(type::Type t, ir::RegOr<ir::addr_t> addr) {
    t.visit(*this, addr);
  }

  void EmitDestroy(type::Array const *t, ir::RegOr<ir::addr_t> addr) {
    if (not t->HasDestructor()) { return; }
    auto [fn, inserted] =
        context().ir().InsertDestroy(t, [&](ir::Subroutine &s) {
          push_current(&s);
          absl::Cleanup c = [&] { state().current.pop_back(); };

          current_block() = s.entry();
          current_block() =
              OnEachArrayElement(current(), t, ir::Reg::Parameter(0),
                                 [=](ir::BasicBlock *entry, ir::Reg reg) {
                                   current_block() = entry;
                                   EmitDestroy(t->data_type(), reg);
                                   return current_block();
                                 });
          current_block()->set_jump(ir::JumpCmd::Return());
        });

    // TODO: Remove const_cast.
    if (inserted) { const_cast<type::Array *>(t)->SetDestructor(fn); }
    current_block()->Append(ir::DestroyInstruction{.type = t, .addr = addr});
  }
  void EmitDestroy(type::Enum const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::Flags const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::Function const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::Pointer const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::Generic const *t, ir::RegOr<ir::addr_t> addr) {
    // TODO
  }
  void EmitDestroy(type::BufferPointer const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::Primitive const *t, ir::RegOr<ir::addr_t> addr) {
    if (type::Type(t) == type::Integer) {
      current_block()->Append(
          ir::CompileTime<ir::Action::Destroy, ir::Integer>{.addr = addr});
    }
  }
  void EmitDestroy(type::Slice const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::Struct const *t, ir::RegOr<ir::addr_t> addr) {
    if (not t->HasDestructor()) { return; }
    current_block()->Append(ir::DestroyInstruction{.type = t, .addr = addr});
  }
  void EmitDestroy(type::Variable const *t, ir::RegOr<ir::addr_t> addr) {
    NOT_YET();
  }
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_EMIT_DESTROY_H
