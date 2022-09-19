#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_INSTRUCTION_SET_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_INSTRUCTION_SET_H

#include "core/type_system/builtin.h"
#include "core/type_system/function.h"
#include "core/type_system/parameter.h"
#include "core/type_system/pointer.h"
#include "core/type_system/type_system.h"
#include "jasmin/function.h"
#include "jasmin/instructions/core.h"

namespace semantic_analysis {
namespace internal_byte_code {

using InstructionSet = jasmin::MakeInstructionSet<jasmin::Push>;

}  // namespace internal_byte_code

using IrFunction = jasmin::Function<internal_byte_code::InstructionSet>;

using TypeSystem = core::TypeSystem<core::BuiltinType, core::ParameterType,
                                    core::PointerType, core::FunctionType>;

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_INSTRUCTION_SET_H
