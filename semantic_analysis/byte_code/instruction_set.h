#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_INSTRUCTION_SET_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_INSTRUCTION_SET_H

#include "jasmin/function.h"
#include "jasmin/instructions/core.h"
#include "semantic_analysis/module/builtin.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

struct BuiltinForeign : jasmin::StackMachineInstruction<BuiltinForeign> {
  static void execute(jasmin::ValueStack& value_stack, core::Type t,
                      BuiltinModule* module);
};

namespace internal_byte_code {

// TOOD: core::*Type instructions should be registerable and not required to be
// explicitly added here.
using InstructionSet = jasmin::MakeInstructionSet<
    jasmin::Push, TypeSystem::JasminInstructionSet, core::ParameterType::Begin,
    core::ParameterType::Append, core::ParameterType::AppendNamed,
    core::ParameterType::End<TypeSystem>, core::FunctionType::End<TypeSystem>,
    BuiltinForeign>;

}  // namespace internal_byte_code

using IrFunction = jasmin::Function<internal_byte_code::InstructionSet>;

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_INSTRUCTION_SET_H
