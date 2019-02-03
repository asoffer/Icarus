#ifndef ICARUS_IR_OUT_PARAMS_H
#define ICARUS_IR_OUT_PARAMS_H

#include "base/container/vector.h"
#include "ir/register.h"

namespace type {
struct Type;
}  // namespace type

namespace ir {
// Represents an output parameter. The boolean value denotes whether the
// register is a register to be filled with the value, or it is the address to
// which the value should be written.
struct OutParams {
  Register AppendReg(type::Type const *);
  void AppendLoc(Register reg);
  size_t size() const { return is_loc_.size(); }

  base::vector<Register> regs_;
  base::vector<bool> is_loc_;
};
}  // namespace ir

#endif  // ICARUS_IR_OUT_PARAMS_H
