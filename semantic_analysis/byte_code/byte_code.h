#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_BYTE_CODE_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_BYTE_CODE_H

#include "ast/expression.h"
#include "semantic_analysis/byte_code/instruction_set.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

// Returns an `IrFunction` accepting no parameters and whose execution computes
// the value associated with `expression`.
IrFunction EmitByteCode(ast::Expression const& expression,
                        Context const& context, TypeSystem& type_system);

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_BYTE_CODE_H
