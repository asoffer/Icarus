#ifndef ICARUS_IR_BUILTIN_MODULE_H
#define ICARUS_IR_BUILTIN_MODULE_H

#include <span>
#include <string_view>

#include "ir/module.h"

namespace ic {

Module BuiltinModule();

jasmin::ProgramFragment<InstructionSet> const& BuiltinFunctionFragment();

std::span<std::string const> BuiltinNames();

}  // namespace ic

#endif  // ICARUS_IR_BUILTIN_MODULE_H
