#ifndef ICARUS_COMPILER_EMIT_DESTROY_H
#define ICARUS_COMPILER_EMIT_DESTROY_H

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
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"

namespace compiler {

struct DestructionEmitter : CompilationDataReference {
  using signature = void(ir::RegOr<ir::addr_t>);

  explicit DestructionEmitter(CompilationDataReference ref)
      : CompilationDataReference(ref) {}

  void operator()(type::Type t, ir::RegOr<ir::addr_t> addr) {
    EmitDestroy(t, addr);
  }
  void operator()(auto const *t, ir::RegOr<ir::addr_t> addr) {
    operator()(type::Type(t), addr);
  }
  void EmitDestroy(type::Type t, ir::RegOr<ir::addr_t> addr) {
    t.visit(*this, addr);
  }

  void EmitDestroy(type::Array const *t, ir::RegOr<ir::addr_t> addr) {
    if (not t->HasDestructor()) { return; }
    auto [fn, inserted] = context().ir().InsertDestroy(t);
    if (inserted) {
      push_current(&*fn);
      absl::Cleanup c = [&] { state().current.pop_back(); };

      current_block() = fn->entry();
      OnEachArrayElement(current(), t, ir::Reg::Arg(0), [=](ir::Reg reg) {
        EmitDestroy(t->data_type(), reg);
      });
      current_block()->set_jump(ir::JumpCmd::Return());
      context().ir().WriteByteCode<EmitByteCode>(fn);
      // TODO: Remove const_cast.
      const_cast<type::Array *>(t)->SetDestructor(fn);
    }
    current_block()->Append(ir::DestroyInstruction{.type = t, .addr = addr});
  }
  void EmitDestroy(type::Enum const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::Flags const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::Function const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::Pointer const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::BufferPointer const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::Primitive const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::Slice const *t, ir::RegOr<ir::addr_t> addr) {}
  void EmitDestroy(type::Struct const *t, ir::RegOr<ir::addr_t> addr) {
    if (not t->HasDestructor()) { return; }
    current_block()->Append(ir::DestroyInstruction{.type = t, .addr = addr});
  }
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_EMIT_DESTROY_H
