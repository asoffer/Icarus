#ifndef ICARUS_IR_INSTRUCTION_INLINER_H
#define ICARUS_IR_INSTRUCTION_INLINER_H

#include <concepts>

#include "base/meta.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/result_buffer.h"

namespace ir {

// Instruction traversal which replaces register values so that the instruction
// can be inlined from one block group into another, avoiding register collisions.
struct Inliner {
  explicit Inliner(size_t register_offset, size_t num_params)
      : register_offset_(register_offset), num_params_(num_params) {}
  void operator()(ir::Reg &r) {
    if (r.is_arg()) {
      r = Reg(r.arg_value() + register_offset_);
    } else {
      r = Reg(r.value() + register_offset_ + num_params_);
    }
  }

  void operator()(base::is_enum auto &) {}
  void operator()(std::integral auto &) {}
  void operator()(std::floating_point auto &) {}
  void operator()(void const *) {}

  void operator()(ir::PartialResultBuffer &buffer) {
    for (size_t i= 0; i < buffer.num_entries(); ++i) {
      if (not buffer[i].is_register()) { continue; }
      Reg reg = buffer[i].get<Reg>();
      (*this)(reg);
    }
  }

  void operator()(base::is_a<ir::RegOr> auto &r) {
    if (not r.is_reg()) { return; }
    Reg reg = r.reg();
    (*this)(reg);
    r = reg;
  }

 private:
  size_t register_offset_;
  size_t num_params_;  // The number of parameters in the to-be-inlined group.
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_INLINER_H
