#include "ir/out_params.h"

namespace ir {
Register Reserve(type::Type const *t);

void OutParams::AppendLoc(Register reg) {
  regs_.push_back(reg);
  is_loc_.push_back(true);
}

Register OutParams::AppendReg(type::Type const *t) {
  auto reg = Reserve(t);
  regs_.push_back(reg);
  is_loc_.push_back(false);
  return reg;
}

}  // namespace ir
