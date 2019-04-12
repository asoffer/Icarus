#ifndef ICARUS_IR_OUT_PARAMS_H
#define ICARUS_IR_OUT_PARAMS_H

#include <vector>
#include "ir/register.h"

namespace type {
struct Type;
}  // namespace type

namespace ir {
// Represents an output parameter. The boolean value denotes whether the
// register is a register to be filled with the value, or it is the address to
// which the value should be written.
struct OutParams {
  Reg AppendReg(type::Type const *);
  void AppendLoc(Reg reg);
  size_t size() const { return is_loc_.size(); }

  std::vector<Reg> regs_;
  std::vector<bool> is_loc_;
};
}  // namespace ir

#endif  // ICARUS_IR_OUT_PARAMS_H
