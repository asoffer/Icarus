#ifndef ICARUS_IR_SCOPE_H
#define ICARUS_IR_SCOPE_H

#include "ir/function.h"

namespace ic {

struct Scope {
  IrFunction& implementation() { return implementation_; }

  void AppendTo(IrFunction& f) const;

  private:
   IrFunction implementation_{1, 0};
};

}  // namespace ic

#endif  // ICARUS_IR_SCOPE_H
