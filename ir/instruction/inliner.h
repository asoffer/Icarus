#ifndef ICARUS_IR_INSTRUCTION_INLINER_H
#define ICARUS_IR_INSTRUCTION_INLINER_H

#include <concepts>

#include "base/meta.h"
#include "ir/value/module_id.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/result_buffer.h"

namespace ir {

// Instruction traversal which replaces register values so that the instruction
// can be inlined from one subroutine into another, avoiding register
// collisions.
struct Inliner {
  explicit Inliner(size_t register_offset, size_t num_params,
                   size_t num_stack_allocations)
      : register_offset_(register_offset),
        num_params_(num_params),
        num_stack_allocations_(num_stack_allocations) {}
  void operator()(ir::Reg &r) {
    switch (r.kind()) {
      case ir::Reg::Kind::Parameter:
        r = Reg(r.as<ir::Reg::Kind::Parameter>() + register_offset_);
        break;
      case ir::Reg::Kind::Output:
      case ir::Reg::Kind::Value:
        r = Reg(r.as<ir::Reg::Kind::Value>() + register_offset_ + num_params_);
        break;
      case ir::Reg::Kind::StackAllocation:
        r = Reg::StackAllocation(r.as<ir::Reg::Kind::StackAllocation>() +
                                 num_stack_allocations_);
    }
  }

  void operator()(base::is_enum auto &) {}
  void operator()(std::integral auto &) {}
  void operator()(std::floating_point auto &) {}
  void operator()(ModuleId &) {}
  void operator()(void const *) {}

  void operator()(ir::PartialResultBuffer &buffer) {
    for (size_t i = 0; i < buffer.num_entries(); ++i) {
      if (not buffer[i].is_register()) { continue; }
      Reg reg = buffer[i].get<Reg>();
      (*this)(reg);
      buffer.set_register(i, reg);
    }
  }

  template <base::is_a<ir::RegOr> R>
  void operator()(R &r) {
    if (not r.is_reg()) { return; }
    Reg reg = r.reg();
    (*this)(reg);
    r = reg;
  }

 private:
  size_t register_offset_;
  size_t num_params_;  // The number of parameters in the to-be-inlined
                       // subroutine.
  size_t num_stack_allocations_;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_INLINER_H
