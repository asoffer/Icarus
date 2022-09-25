#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_INSTRUCTION_SET_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_INSTRUCTION_SET_H

#include "jasmin/function.h"
#include "jasmin/instructions/core.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {
namespace internal_byte_code {

using InstructionSet =
    jasmin::MakeInstructionSet<jasmin::Push, TypeSystem::JasminInstructionSet>;

}  // namespace internal_byte_code

using IrFunction = jasmin::Function<internal_byte_code::InstructionSet>;

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_INSTRUCTION_SET_H
