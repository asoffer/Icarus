#ifndef ICARUS_IR_PROGRAM_ARGUMENTS_H
#define ICARUS_IR_PROGRAM_ARGUMENTS_H

#include <string>
#include <vector>

#include "common/slice.h"

namespace ic {

void SetProgramArguments(std::vector<std::string> arguments);
Slice ProgramArguments();

}  // namespace ic

#endif  // ICARUS_IR_PROGRAM_ARGUMENTS_H
