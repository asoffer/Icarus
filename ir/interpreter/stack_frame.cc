#include "ir/interpreter/stack_frame.h"

#include "ir/interpreter/architecture.h"
#include "type/function.h"
#include "type/type.h"

namespace interpreter {

namespace {

size_t NumRegisters(ir::Fn fn) {
  switch (fn.kind()) {
    case ir::Fn::Kind::Native: return fn.native()->num_regs();
    default: return 0;
  }
}

size_t NumOutputs(ir::Fn fn) { return fn.type()->return_types().size(); }

}  // namespace

StackFrame::StackFrame(ir::Fn fn, base::untyped_buffer &stack)
    : fn_(fn),
      regs_({.num_registers  = NumRegisters(fn_),
             .num_parameters = fn_.num_parameters(),
             .num_outputs    = NumOutputs(fn_)}) {
  if (fn_.kind() == ir::Fn::Kind::Native) {
    core::Bytes next_reg_loc = core::Bytes(stack.size());
    fn_.native()->for_each_alloc([&](type::Type t, ir::Reg r) {
      ASSERT(t.valid() == true);
      next_reg_loc = core::FwdAlign(next_reg_loc, t.alignment(kArchitecture));
      regs_.set(r, ir::Addr::Stack(next_reg_loc.value()));
      next_reg_loc += t.bytes(kArchitecture);
    });
    stack.append_bytes(next_reg_loc.value() - stack.size());
  }
}

}  // namespace interpreter
