#include "ir/out_params.h"

namespace ir {

Reg Reserve();

void OutParams::AppendLoc(Reg reg) {
  regs_.push_back(reg);
  is_loc_.push_back(true);
}

Reg OutParams::AppendReg(type::Type const *t) {
  auto reg = Reserve();
  regs_.push_back(reg);
  is_loc_.push_back(false);
  return reg;
}

}  // namespace ir
