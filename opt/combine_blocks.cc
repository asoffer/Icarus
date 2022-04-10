#include "opt/combine_blocks.h"

#include <queue>
#include <type_traits>
#include <utility>

#include "base/log.h"
#include "ir/subroutine.h"
#include "ir/instruction/core.h"

namespace opt {

// TODO: Implement.
void ReduceEmptyBlocks(ir::Subroutine* fn) {}
void CombineBlocks(ir::Subroutine* fn) {}
void RemoveTrivialFunctionCalls(ir::Subroutine* fn) {}

}  // namespace opt
