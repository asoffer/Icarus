#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_INSTRUCTION_SET_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_INSTRUCTION_SET_H

#include "jasmin/function.h"
#include "jasmin/instructions/core.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {
// Defined in "semantic_analysis/byte_code/foreign_function_map.h", in the same
// build target.
struct ForeignFunctionMap;

struct BuiltinForeign : jasmin::StackMachineInstruction<BuiltinForeign> {
  static void execute(jasmin::ValueStack& value_stack, core::Type t,
                      ForeignFunctionMap* module);
};

struct InvokeForeignFunction
    : jasmin::StackMachineInstruction<InvokeForeignFunction> {
  static void execute(jasmin::ValueStack& value_stack, void (*fn_ptr)(),
                      core::Parameter<core::Type> const* parameters,
                      size_t parameter_count,
                      core::Type const* maybe_return_type);
};

namespace internal_byte_code {

// TOOD: core::*Type instructions should be registerable and not required to
// be explicitly added here.
using InstructionSet = jasmin::MakeInstructionSet<
    jasmin::Push, TypeSystem::JasminInstructionSet, core::ParameterType::Begin,
    core::ParameterType::Append, core::ParameterType::AppendNamed,
    core::ParameterType::End<TypeSystem>, core::FunctionType::End<TypeSystem>,
    BuiltinForeign, InvokeForeignFunction>;

}  // namespace internal_byte_code

using IrFunction = jasmin::Function<internal_byte_code::InstructionSet>;

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_INSTRUCTION_SET_H
