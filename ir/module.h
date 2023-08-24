#ifndef ICARUS_IR_MODULE_H
#define ICARUS_IR_MODULE_H

#include <span>

#include "jasmin/function.h"
#include "jasmin/instruction.h"
#include "jasmin/instructions/core.h"

namespace ic {

using InstructionSet = jasmin::MakeInstructionSet<jasmin::Push, jasmin::Drop>;
using IrFunction     = jasmin::Function<InstructionSet>;

struct Module {
  IrFunction initializer{0, 0};
};

}  // namespace ic

#endif  // ICARUS_IR_MODULE_H
